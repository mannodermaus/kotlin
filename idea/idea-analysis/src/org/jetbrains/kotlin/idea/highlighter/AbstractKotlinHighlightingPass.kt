/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.highlighter

import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.lang.annotation.AnnotationBuilder
import com.intellij.lang.annotation.AnnotationHolder
import com.intellij.lang.annotation.Annotator
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Document
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.openapi.util.Key
import com.intellij.psi.PsiElement
import com.intellij.util.containers.MultiMap
import org.jetbrains.kotlin.descriptors.DeclarationDescriptor
import org.jetbrains.kotlin.diagnostics.Diagnostic
import org.jetbrains.kotlin.diagnostics.DiagnosticFactory
import org.jetbrains.kotlin.diagnostics.Errors
import org.jetbrains.kotlin.idea.caches.resolve.analyzeWithAllCompilerChecks
import org.jetbrains.kotlin.idea.quickfix.QuickFixes
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtNameReferenceExpression
import org.jetbrains.kotlin.psi.KtParameter
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.types.KotlinType
import java.lang.reflect.*
import java.util.*

abstract class AbstractKotlinHighlightingPass(file: KtFile, document: Document) :
    AbstractBindingContextAwareHighlightingPassBase(file, document) {
    override val annotator: Annotator
        get() = KotlinAfterAnalysisAnnotator()

    private inner class KotlinAfterAnalysisAnnotator : Annotator {
        override fun annotate(element: PsiElement, holder: AnnotationHolder) {
            getAfterAnalysisVisitor(holder, bindingContext()).forEach { visitor -> element.accept(visitor) }
        }
    }

    override fun runAnnotatorWithContext(element: PsiElement, holder: AnnotationHolder) {
        // annotate diagnostics on fly: show diagnostics as soon as front-end reports them
        // don't create quick fixes as it could require some resolve
        val annotationBuilderByDiagnostic = mutableMapOf<Diagnostic, AnnotationBuilder>()
        val analysisResult =
            file.analyzeWithAllCompilerChecks({ annotateDiagnostic(holder, it, annotationBuilderByDiagnostic, true) })
        if (analysisResult.isError()) {
            throw ProcessCanceledException(analysisResult.error)
        }

        // resolve is done!

        val bindingCtx = analysisResult.bindingContext
        bindingContext = bindingCtx
        val diagnostics = bindingCtx.diagnostics

        if (!diagnostics.isEmpty()) {
            // this could happen when resolve has not been run (a cached result is used)
            if (annotationBuilderByDiagnostic.isEmpty()) {
                diagnostics.forEach {
                    annotateDiagnostic(holder, it, annotationBuilderByDiagnostic, false)
                }
            } else {
                // annotate diagnostics with quickfixes when analysis is ready
                diagnostics.forEach {
                    if (annotationBuilderByDiagnostic[it] == null) {
                        annotateDiagnostic(holder, it, annotationBuilderByDiagnostic, false)
                    } else {
                        annotateQuickFixes(it, annotationBuilderByDiagnostic)
                    }
                }
            }
        }

        super.runAnnotatorWithContext(element, holder)
    }

    private fun annotateDiagnostic(
        holder: AnnotationHolder,
        diagnostic: Diagnostic,
        annotationBuilderByDiagnostic: MutableMap<Diagnostic, AnnotationBuilder>? = null,
        noFixes: Boolean = false
    ) = annotateDiagnostic(file, holder, diagnostic, annotationBuilderByDiagnostic, ::shouldSuppressUnusedParameter, noFixes)

    private fun annotateQuickFixes(
        diagnostic: Diagnostic,
        annotationBuilderByDiagnostic: MutableMap<Diagnostic, AnnotationBuilder>
    ) {
        val element = diagnostic.psiElement

        val shouldHighlightErrors =
            KotlinHighlightingUtil.shouldHighlightErrors(
                if (element.isPhysical) file else element
            )

        if (shouldHighlightErrors) {
            ElementAnnotator(element) { param ->
                shouldSuppressUnusedParameter(param)
            }.registerDiagnosticsQuickFixes(diagnostic, annotationBuilderByDiagnostic)
        }
    }


    protected open fun shouldSuppressUnusedParameter(parameter: KtParameter): Boolean = false

    companion object {
        fun createQuickFixes(diagnostic: Diagnostic): Collection<IntentionAction> =
            createQuickFixes(listOfNotNull(diagnostic))[diagnostic]

        private val UNRESOLVED_KEY = Key<Unit>("KotlinHighlightingPass.UNRESOLVED_KEY")

        fun wasUnresolved(element: KtNameReferenceExpression) = element.getUserData(UNRESOLVED_KEY) != null

        fun getAfterAnalysisVisitor(holder: AnnotationHolder, bindingContext: BindingContext) = arrayOf(
            PropertiesHighlightingVisitor(holder, bindingContext),
            FunctionsHighlightingVisitor(holder, bindingContext),
            VariablesHighlightingVisitor(holder, bindingContext),
            TypeKindHighlightingVisitor(holder, bindingContext)
        )

        fun annotateDiagnostic(
            file: KtFile,
            holder: AnnotationHolder,
            diagnostic: Diagnostic,
            annotationBuilderByDiagnostic: MutableMap<Diagnostic, AnnotationBuilder>? = null,
            shouldSuppressUnusedParameter: (KtParameter) -> Boolean = { false },
            noFixes: Boolean = false
        ) {
            val element = diagnostic.psiElement
            if (element is KtNameReferenceExpression) {
                val unresolved = diagnostic.factory == Errors.UNRESOLVED_REFERENCE
                element.putUserData(UNRESOLVED_KEY, if (unresolved) Unit else null)
            }

            val shouldHighlightErrors =
                KotlinHighlightingUtil.shouldHighlightErrors(
                    if (element.isPhysical) file else element
                )

            if (shouldHighlightErrors) {
                val elementAnnotator = ElementAnnotator(element) { param ->
                    shouldSuppressUnusedParameter(param)
                }
                elementAnnotator.registerDiagnosticsAnnotations(holder, diagnostic, annotationBuilderByDiagnostic, noFixes)
            }
        }
    }
}


internal fun createQuickFixes(similarDiagnostics: Collection<Diagnostic>): MultiMap<Diagnostic, IntentionAction> {
    val first = similarDiagnostics.minByOrNull { it.toString() }
    val factory = similarDiagnostics.first().getRealDiagnosticFactory()

    val actions = MultiMap<Diagnostic, IntentionAction>()

    val intentionActionsFactories = QuickFixes.getInstance().getActionFactories(factory)
    for (intentionActionsFactory in intentionActionsFactories) {
        val allProblemsActions = intentionActionsFactory.createActionsForAllProblems(similarDiagnostics)
        if (allProblemsActions.isNotEmpty()) {
            actions.putValues(first, allProblemsActions)
        } else {
            for (diagnostic in similarDiagnostics) {
                actions.putValues(diagnostic, intentionActionsFactory.createActions(diagnostic))
            }
        }
    }

    for (diagnostic in similarDiagnostics) {
        actions.putValues(diagnostic, QuickFixes.getInstance().getActions(diagnostic.factory))
    }

    actions.values().forEach { NoDeclarationDescriptorsChecker.check(it::class.java) }

    return actions
}

private fun Diagnostic.getRealDiagnosticFactory(): DiagnosticFactory<*> =
    when (factory) {
        Errors.PLUGIN_ERROR -> Errors.PLUGIN_ERROR.cast(this).a.factory
        Errors.PLUGIN_WARNING -> Errors.PLUGIN_WARNING.cast(this).a.factory
        Errors.PLUGIN_INFO -> Errors.PLUGIN_INFO.cast(this).a.factory
        else -> factory
    }

private object NoDeclarationDescriptorsChecker {
    private val LOG = Logger.getInstance(NoDeclarationDescriptorsChecker::class.java)

    private val checkedQuickFixClasses = Collections.synchronizedSet(HashSet<Class<*>>())

    fun check(quickFixClass: Class<*>) {
        if (!checkedQuickFixClasses.add(quickFixClass)) return

        for (field in quickFixClass.declaredFields) {
            checkType(field.genericType, field)
        }

        quickFixClass.superclass?.let { check(it) }
    }

    private fun checkType(type: Type, field: Field) {
        when (type) {
            is Class<*> -> {
                if (DeclarationDescriptor::class.java.isAssignableFrom(type) || KotlinType::class.java.isAssignableFrom(type)) {
                    LOG.error(
                        "QuickFix class ${field.declaringClass.name} contains field ${field.name} that holds ${type.simpleName}. "
                                + "This leads to holding too much memory through this quick-fix instance. "
                                + "Possible solution can be wrapping it using KotlinIntentionActionFactoryWithDelegate."
                    )
                }

                if (IntentionAction::class.java.isAssignableFrom(type)) {
                    check(type)
                }

            }

            is GenericArrayType -> checkType(type.genericComponentType, field)

            is ParameterizedType -> {
                if (Collection::class.java.isAssignableFrom(type.rawType as Class<*>)) {
                    type.actualTypeArguments.forEach { checkType(it, field) }
                }
            }

            is WildcardType -> type.upperBounds.forEach { checkType(it, field) }
        }
    }
}
