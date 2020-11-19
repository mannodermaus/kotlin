/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.highlighter

import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.codeInspection.ProblemHighlightType
import com.intellij.lang.annotation.AnnotationBuilder
import com.intellij.lang.annotation.AnnotationHolder
import com.intellij.openapi.diagnostic.ControlFlowException
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.colors.CodeInsightColors
import com.intellij.psi.MultiRangeReference
import com.intellij.psi.PsiElement
import com.intellij.util.containers.MultiMap
import org.jetbrains.kotlin.cli.common.arguments.K2JVMCompilerArguments
import org.jetbrains.kotlin.config.KotlinFacetSettingsProvider
import org.jetbrains.kotlin.diagnostics.Diagnostic
import org.jetbrains.kotlin.diagnostics.Errors
import org.jetbrains.kotlin.diagnostics.Severity
import org.jetbrains.kotlin.idea.references.mainReference
import org.jetbrains.kotlin.idea.util.module
import org.jetbrains.kotlin.psi.KtParameter
import org.jetbrains.kotlin.psi.KtReferenceExpression

internal class ElementAnnotator(
    private val element: PsiElement,
    private val shouldSuppressUnusedParameter: (KtParameter) -> Boolean
) {
    fun registerDiagnosticsAnnotations(holder: AnnotationHolder, diagnostics: Collection<Diagnostic>, noFixes: Boolean) {
        diagnostics.groupBy { it.factory }.forEach { registerDiagnosticAnnotations(holder, it.value, noFixes) }
    }

    fun registerDiagnosticsAnnotations(
        holder: AnnotationHolder,
        diagnostic: Diagnostic,
        annotationBuilderByDiagnostic: MutableMap<Diagnostic, AnnotationBuilder>? = null,
        noFixes: Boolean
    ) {
        // hack till the root cause #KT-21246 is fixed
        if (isIrCompileClassDiagnosticForModulesWithEnabledIR(diagnostic)) return

        val presentationInfo = presentationInfo(diagnostic) ?: return
        setUpAnnotations(holder, listOf(diagnostic), presentationInfo, annotationBuilderByDiagnostic, noFixes)
    }

    fun registerDiagnosticsQuickFixes(
        diagnostic: Diagnostic,
        annotationBuilderByDiagnostic: MutableMap<Diagnostic, AnnotationBuilder>
    ) {
        // hack till the root cause #KT-21246 is fixed
        if (isIrCompileClassDiagnosticForModulesWithEnabledIR(diagnostic)) return

        val presentationInfo = presentationInfo(diagnostic) ?: return
        val annotationBuilder = annotationBuilderByDiagnostic[diagnostic] ?: return

        val diagnostics = listOf(diagnostic)
        val fixesMap = createFixesMap(diagnostics) ?: return

        presentationInfo.applyFixes(fixesMap, diagnostic, annotationBuilder)
    }

    private fun registerDiagnosticAnnotations(holder: AnnotationHolder, diagnostics: List<Diagnostic>, noFixes: Boolean) {
        assert(diagnostics.isNotEmpty())

        val validDiagnostics = diagnostics.filter { it.isValid }
        if (validDiagnostics.isEmpty()) return

        val diagnostic = diagnostics.first()
        val factory = diagnostic.factory

        // hack till the root cause #KT-21246 is fixed
        if (isIrCompileClassDiagnosticForModulesWithEnabledIR(diagnostic)) return

        assert(diagnostics.all { it.psiElement == element && it.factory == factory })

        val presentationInfo = presentationInfo(diagnostic) ?: return

        setUpAnnotations(holder, diagnostics, presentationInfo, null, noFixes)
    }

    private fun presentationInfo(diagnostic: Diagnostic): AnnotationPresentationInfo? {
        val factory = diagnostic.factory
        val ranges = diagnostic.textRanges
        val presentationInfo: AnnotationPresentationInfo = when (factory.severity) {
            Severity.ERROR -> {
                when (factory) {
                    in Errors.UNRESOLVED_REFERENCE_DIAGNOSTICS -> {
                        val referenceExpression = element as KtReferenceExpression
                        val reference = referenceExpression.mainReference
                        if (reference is MultiRangeReference) {
                            AnnotationPresentationInfo(
                                ranges = reference.ranges.map { it.shiftRight(referenceExpression.textOffset) },
                                highlightType = ProblemHighlightType.LIKE_UNKNOWN_SYMBOL
                            )
                        } else {
                            AnnotationPresentationInfo(ranges, highlightType = ProblemHighlightType.LIKE_UNKNOWN_SYMBOL)
                        }
                    }

                    Errors.ILLEGAL_ESCAPE -> AnnotationPresentationInfo(
                        ranges, textAttributes = KotlinHighlightingColors.INVALID_STRING_ESCAPE
                    )

                    Errors.REDECLARATION -> AnnotationPresentationInfo(
                        ranges = listOf(diagnostic.textRanges.first()), nonDefaultMessage = ""
                    )

                    else -> {
                        AnnotationPresentationInfo(
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

                AnnotationPresentationInfo(
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
            Severity.INFO -> AnnotationPresentationInfo(ranges, highlightType = ProblemHighlightType.INFORMATION)
        }
        return presentationInfo
    }

    private fun setUpAnnotations(
        holder: AnnotationHolder,
        diagnostics: List<Diagnostic>,
        data: AnnotationPresentationInfo,
        annotationBuilderByDiagnostic: MutableMap<Diagnostic, AnnotationBuilder>? = null,
        noFixes: Boolean
    ) {
        val fixesMap =
            createFixesMap(diagnostics, noFixes)

        data.processDiagnostics(holder, diagnostics, annotationBuilderByDiagnostic, fixesMap)
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
        val LOG = Logger.getInstance(ElementAnnotator::class.java)
    }
}
