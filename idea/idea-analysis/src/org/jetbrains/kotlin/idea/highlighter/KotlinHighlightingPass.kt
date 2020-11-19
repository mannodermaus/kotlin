/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.highlighter

import com.intellij.codeHighlighting.*
import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.lang.annotation.AnnotationBuilder
import com.intellij.lang.annotation.AnnotationHolder
import com.intellij.lang.annotation.Annotator
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Key
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import org.jetbrains.kotlin.diagnostics.Diagnostic
import org.jetbrains.kotlin.diagnostics.Errors
import org.jetbrains.kotlin.idea.caches.resolve.analyzeWithAllCompilerChecks
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtNameReferenceExpression
import org.jetbrains.kotlin.psi.KtParameter
import org.jetbrains.kotlin.resolve.BindingContext

class KotlinHighlightingPass(file: KtFile, document: Document) : AbstractKotlinHighlightingPass(file, document) {
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
        val analysisResult = file.analyzeWithAllCompilerChecks({ annotateDiagnostic(holder, it, annotationBuilderByDiagnostic, true) })
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
                    annotateQuickFixes(it, annotationBuilderByDiagnostic)
                }
            }
        }

        super.runAnnotatorWithContext(element, holder)
    }

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

    private fun annotateDiagnostic(
        holder: AnnotationHolder,
        diagnostic: Diagnostic,
        annotationBuilderByDiagnostic: MutableMap<Diagnostic, AnnotationBuilder>? = null,
        noFixes: Boolean
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

    }

    class Factory : TextEditorHighlightingPassFactory {
        override fun createHighlightingPass(file: PsiFile, editor: Editor): TextEditorHighlightingPass? {
            if (file !is KtFile) return null
            return KotlinHighlightingPass(file, editor.document)
        }
    }

    class Registrar : TextEditorHighlightingPassFactoryRegistrar {
        override fun registerHighlightingPassFactory(registrar: TextEditorHighlightingPassRegistrar, project: Project) {
            registrar.registerTextEditorHighlightingPass(
                Factory(),
                null,
                intArrayOf(Pass.UPDATE_ALL),
                false,
                -1
            )
        }
    }

}