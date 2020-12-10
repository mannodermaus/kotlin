/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.highlighter

import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.codeInspection.ProblemHighlightType
import com.intellij.openapi.diagnostic.ControlFlowException
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.colors.CodeInsightColors
import com.intellij.psi.MultiRangeReference
import com.intellij.psi.PsiElement
import com.intellij.util.containers.MultiMap
import org.jetbrains.kotlin.cli.common.arguments.K2JVMCompilerArguments
import org.jetbrains.kotlin.config.KotlinFacetSettingsProvider
import org.jetbrains.kotlin.descriptors.DeclarationDescriptor
import org.jetbrains.kotlin.diagnostics.Diagnostic
import org.jetbrains.kotlin.diagnostics.DiagnosticFactory
import org.jetbrains.kotlin.diagnostics.Errors
import org.jetbrains.kotlin.diagnostics.Severity
import org.jetbrains.kotlin.idea.quickfix.QuickFixes
import org.jetbrains.kotlin.idea.references.mainReference
import org.jetbrains.kotlin.idea.util.module
import org.jetbrains.kotlin.psi.KtParameter
import org.jetbrains.kotlin.psi.KtReferenceExpression
import org.jetbrains.kotlin.types.KotlinType
import java.lang.reflect.*
import java.util.*

internal class ElementHighlighter(
    private val element: PsiElement,
    private val shouldSuppressUnusedParameter: (KtParameter) -> Boolean
) {
    fun registerDiagnosticsHighlightInfos(
        highlightInfoWrapper: HighlightInfoWrapper,
        diagnostic: Diagnostic,
        infosByDiagnostic: MutableMap<Diagnostic, HighlightInfo>? = null,
        noFixes: Boolean
    ) {
        // hack till the root cause #KT-21246 is fixed
        if (isIrCompileClassDiagnosticForModulesWithEnabledIR(diagnostic)) return

        val presentationInfo = presentationInfo(diagnostic) ?: return
        setUpHighlightInfos(highlightInfoWrapper, listOf(diagnostic), presentationInfo, infosByDiagnostic, noFixes)
    }

    fun registerDiagnosticsQuickFixes(
        diagnostic: Diagnostic,
        infosByDiagnostic: Map<Diagnostic, HighlightInfo>
    ) {
        // hack till the root cause #KT-21246 is fixed
        if (isIrCompileClassDiagnosticForModulesWithEnabledIR(diagnostic)) return

        val presentationInfo = presentationInfo(diagnostic) ?: return
        val highlightInfo = infosByDiagnostic[diagnostic] ?: return

        val diagnostics = listOf(diagnostic)
        val fixesMap = createFixesMap(diagnostics) ?: return

        presentationInfo.applyFixes(fixesMap, diagnostic, highlightInfo)
    }

    private fun presentationInfo(diagnostic: Diagnostic): HighlightPresentationInfo? {
        val factory = diagnostic.factory
        val ranges = diagnostic.textRanges
        val presentationInfo: HighlightPresentationInfo = when (factory.severity) {
            Severity.ERROR -> {
                when (factory) {
                    in Errors.UNRESOLVED_REFERENCE_DIAGNOSTICS -> {
                        val referenceExpression = element as KtReferenceExpression
                        val reference = referenceExpression.mainReference
                        if (reference is MultiRangeReference) {
                            HighlightPresentationInfo(
                                ranges = reference.ranges.map { it.shiftRight(referenceExpression.textOffset) },
                                highlightType = ProblemHighlightType.LIKE_UNKNOWN_SYMBOL
                            )
                        } else {
                            HighlightPresentationInfo(ranges, highlightType = ProblemHighlightType.LIKE_UNKNOWN_SYMBOL)
                        }
                    }

                    Errors.ILLEGAL_ESCAPE -> HighlightPresentationInfo(
                        ranges, textAttributes = KotlinHighlightingColors.INVALID_STRING_ESCAPE
                    )

                    Errors.REDECLARATION -> HighlightPresentationInfo(
                        ranges = listOf(diagnostic.textRanges.first()), nonDefaultMessage = ""
                    )

                    else -> {
                        HighlightPresentationInfo(
                            ranges,
                            highlightType = if (factory == Errors.INVISIBLE_REFERENCE)
                                ProblemHighlightType.LIKE_UNKNOWN_SYMBOL
                            else
                                null
                        )
                    }
                }
            }
            Severity.WARNING -> {
                if (factory == Errors.UNUSED_PARAMETER && shouldSuppressUnusedParameter(element as KtParameter)) {
                    return null
                }

                HighlightPresentationInfo(
                    ranges,
                    textAttributes = when (factory) {
                        Errors.DEPRECATION -> CodeInsightColors.DEPRECATED_ATTRIBUTES
                        Errors.UNUSED_ANONYMOUS_PARAMETER -> CodeInsightColors.WEAK_WARNING_ATTRIBUTES
                        else -> null
                    },
                    highlightType = when (factory) {
                        in Errors.UNUSED_ELEMENT_DIAGNOSTICS -> ProblemHighlightType.LIKE_UNUSED_SYMBOL
                        Errors.UNUSED_ANONYMOUS_PARAMETER -> ProblemHighlightType.WEAK_WARNING
                        else -> null
                    }
                )
            }
            Severity.INFO -> HighlightPresentationInfo(ranges, highlightType = ProblemHighlightType.INFORMATION)
        }
        return presentationInfo
    }

    private fun setUpHighlightInfos(
        highlightInfoWrapper: HighlightInfoWrapper,
        diagnostics: List<Diagnostic>,
        data: HighlightPresentationInfo,
        infosByDiagnostic: MutableMap<Diagnostic, HighlightInfo>? = null,
        noFixes: Boolean
    ) {
        val fixesMap =
            createFixesMap(diagnostics, noFixes)

        data.processDiagnostics(highlightInfoWrapper, diagnostics, infosByDiagnostic, fixesMap)
    }

    private fun createFixesMap(
        diagnostics: List<Diagnostic>,
        noFixes: Boolean = false
    ): MultiMap<Diagnostic, IntentionAction>? {
        val fixesMap =
            if (noFixes) {
                null
            } else {
                try {
                    createQuickFixes(diagnostics)
                } catch (e: Exception) {
                    if (e is ControlFlowException) {
                        throw e
                    }
                    LOG.error(e)
                    MultiMap<Diagnostic, IntentionAction>()
                }
            }
        return fixesMap
    }

    private fun isIrCompileClassDiagnosticForModulesWithEnabledIR(diagnostic: Diagnostic): Boolean {
        if (diagnostic.factory != Errors.IR_COMPILED_CLASS) return false
        val module = element.module ?: return false
        val moduleFacetSettings = KotlinFacetSettingsProvider.getInstance(element.project)?.getSettings(module) ?: return false
        return moduleFacetSettings.isCompilerSettingPresent(K2JVMCompilerArguments::useIR)
                || moduleFacetSettings.isCompilerSettingPresent(K2JVMCompilerArguments::allowJvmIrDependencies)
    }

    companion object {
        val LOG = Logger.getInstance(ElementHighlighter::class.java)
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
