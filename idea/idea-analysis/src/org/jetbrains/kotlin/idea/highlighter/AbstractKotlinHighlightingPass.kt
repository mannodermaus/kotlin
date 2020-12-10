/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.highlighter

import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.openapi.editor.Document
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.openapi.util.Key
import com.intellij.psi.PsiElement
import org.jetbrains.kotlin.diagnostics.Diagnostic
import org.jetbrains.kotlin.diagnostics.Errors
import org.jetbrains.kotlin.idea.caches.resolve.analyzeWithAllCompilerChecks
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtNameReferenceExpression
import org.jetbrains.kotlin.psi.KtParameter
import org.jetbrains.kotlin.resolve.BindingContext

abstract class AbstractKotlinHighlightingPass(file: KtFile, document: Document) :
    AbstractBindingContextAwareHighlightingPassBase(file, document) {
    private var infosByDiagnostic = mutableMapOf<Diagnostic, HighlightInfo>()

    override val highlighter: Highlighter
        get() = KotlinAfterAnalysisHighlighter()

    private inner class KotlinAfterAnalysisHighlighter : Highlighter {
        override fun highlight(element: PsiElement, highlightInfoWrapper: HighlightInfoWrapper) {
            getAfterAnalysisVisitor(highlightInfoWrapper, bindingContext()).forEach { visitor -> element.accept(visitor) }
        }
    }

    override fun doCollectInformation(progress: ProgressIndicator) {
        try {
            super.doCollectInformation(progress)
        } finally {
            infosByDiagnostic.clear()
        }
    }

    override fun buildBindingContext(highlightInfoWrapper: HighlightInfoWrapper): BindingContext {
        // annotate diagnostics on fly: show diagnostics as soon as front-end reports them
        // don't create quick fixes as it could require some resolve
        val analysisResult =
            file.analyzeWithAllCompilerChecks({ highlightDiagnostic(highlightInfoWrapper, it, infosByDiagnostic, true) })
        if (analysisResult.isError()) {
            throw ProcessCanceledException(analysisResult.error)
        }

        // resolve is done!

        return analysisResult.bindingContext
    }

    override fun runAnnotatorWithContext(element: PsiElement, highlightInfoWrapper: HighlightInfoWrapper) {
        val diagnostics = bindingContext().diagnostics

        if (!diagnostics.isEmpty()) {
            // this could happen when resolve has not been run (a cached result is used)
            if (infosByDiagnostic.isEmpty()) {
                diagnostics.forEach {
                    highlightDiagnostic(highlightInfoWrapper, it, infosByDiagnostic)
                }
            } else {
                // annotate diagnostics with quickfixes when analysis is ready
                diagnostics.forEach {
                    highlightQuickFixes(it, infosByDiagnostic)
                }
            }
        }
        super.runAnnotatorWithContext(element, highlightInfoWrapper)
    }

    private fun highlightDiagnostic(
        highlightInfoWrapper: HighlightInfoWrapper,
        diagnostic: Diagnostic,
        infosByDiagnostic: MutableMap<Diagnostic, HighlightInfo>,
        noFixes: Boolean = false
    ) = highlightDiagnostic(file, highlightInfoWrapper, diagnostic, infosByDiagnostic, ::shouldSuppressUnusedParameter, noFixes)

    private fun highlightQuickFixes(
        diagnostic: Diagnostic,
        infosByDiagnostic: Map<Diagnostic, HighlightInfo>
    ) {
        val element = diagnostic.psiElement

        val shouldHighlightErrors =
            KotlinHighlightingUtil.shouldHighlightErrors(
                if (element.isPhysical) file else element
            )

        if (shouldHighlightErrors) {
            ElementHighlighter(element) { param ->
                shouldSuppressUnusedParameter(param)
            }.registerDiagnosticsQuickFixes(diagnostic, infosByDiagnostic)
        }
    }

    protected open fun shouldSuppressUnusedParameter(parameter: KtParameter): Boolean = false

    companion object {
        fun createQuickFixes(diagnostic: Diagnostic): Collection<IntentionAction> =
            createQuickFixes(listOfNotNull(diagnostic))[diagnostic]

        private val UNRESOLVED_KEY = Key<Unit>("KotlinHighlightingPass.UNRESOLVED_KEY")

        fun wasUnresolved(element: KtNameReferenceExpression) = element.getUserData(UNRESOLVED_KEY) != null

        fun getAfterAnalysisVisitor(highlightInfoWrapper: HighlightInfoWrapper, bindingContext: BindingContext) =
            arrayOf(
                PropertiesHighlightingVisitor(highlightInfoWrapper, bindingContext),
                FunctionsHighlightingVisitor(highlightInfoWrapper, bindingContext),
                VariablesHighlightingVisitor(highlightInfoWrapper, bindingContext),
                TypeKindHighlightingVisitor(highlightInfoWrapper, bindingContext)
            )

        internal fun highlightDiagnostic(
            file: KtFile,
            highlightInfoWrapper: HighlightInfoWrapper,
            diagnostic: Diagnostic,
            infosByDiagnostic: MutableMap<Diagnostic, HighlightInfo>? = null,
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
                val elementAnnotator = ElementHighlighter(element) { param ->
                    shouldSuppressUnusedParameter(param)
                }
                elementAnnotator.registerDiagnosticsHighlightInfos(highlightInfoWrapper, diagnostic, infosByDiagnostic, noFixes)
            }
        }
    }

}