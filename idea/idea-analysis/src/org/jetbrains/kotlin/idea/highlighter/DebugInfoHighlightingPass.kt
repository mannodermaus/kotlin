/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.highlighter

import com.intellij.codeHighlighting.*
import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.codeInsight.daemon.impl.HighlightInfoType
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import org.jetbrains.kotlin.checkers.utils.DebugInfoUtil
import org.jetbrains.kotlin.idea.KotlinPluginUtil
import org.jetbrains.kotlin.idea.util.ProjectRootsUtil
import org.jetbrains.kotlin.idea.util.application.isInternal
import org.jetbrains.kotlin.psi.KtCodeFragment
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtReferenceExpression

class DebugInfoHighlightingPass(file: KtFile, document: Document) : AbstractBindingContextAwareHighlightingPassBase(file, document) {
    override val highlighter: Highlighter
        get() = DebugInfoHighlighter()

    private inner class DebugInfoHighlighter : Highlighter {
        override fun highlight(element: PsiElement, highlightInfoWrapper: HighlightInfoWrapper) {
            if (element is KtFile && element !is KtCodeFragment) {
                fun errorAnnotation(
                    expression: PsiElement,
                    message: String,
                    textAttributes: TextAttributesKey? = KotlinHighlightingColors.DEBUG_INFO
                ) {
                    HighlightInfo.newHighlightInfo(HighlightInfoType.ERROR)
                        .description(message)
                        .range(expression.textRange)
                        .also { builder -> textAttributes?.let { builder.textAttributes(it) } }
                        .createUnconditionally()
                        .also(highlightInfoWrapper::add)
                }

                try {
                    DebugInfoUtil.markDebugAnnotations(element, bindingContext(), object : DebugInfoUtil.DebugInfoReporter() {
                        override fun reportElementWithErrorType(expression: KtReferenceExpression) =
                            errorAnnotation(expression, "Resolved to error element", KotlinHighlightingColors.RESOLVED_TO_ERROR)

                        override fun reportMissingUnresolved(expression: KtReferenceExpression) =
                            errorAnnotation(
                                expression,
                                "Reference is not resolved to anything, but is not marked unresolved"
                            )

                        override fun reportUnresolvedWithTarget(expression: KtReferenceExpression, target: String) =
                            errorAnnotation(
                                expression,
                                "Reference marked as unresolved is actually resolved to $target"
                            )
                    })
                } catch (e: ProcessCanceledException) {
                    throw e
                } catch (e: Throwable) {
                    // TODO
                    errorAnnotation(element, e.javaClass.canonicalName + ": " + e.message, null)
                    e.printStackTrace()
                }

            }
        }
    }

    class Factory : TextEditorHighlightingPassFactory {
        override fun createHighlightingPass(file: PsiFile, editor: Editor): TextEditorHighlightingPass? {
            return if (file is KtFile &&
                (isInternal() && (KotlinPluginUtil.isSnapshotVersion() || KotlinPluginUtil.isDevVersion())) &&
                ProjectRootsUtil.isInProjectOrLibSource(file)
            ) {
                DebugInfoHighlightingPass(file, editor.document)
            } else {
                null
            }
        }
    }

    class Registrar : TextEditorHighlightingPassFactoryRegistrar {
        override fun registerHighlightingPassFactory(registrar: TextEditorHighlightingPassRegistrar, project: Project) {
            registrar.registerTextEditorHighlightingPass(
                Factory(),
                intArrayOf(Pass.UPDATE_ALL),
                null,
                false,
                -1
            )
        }
    }

}