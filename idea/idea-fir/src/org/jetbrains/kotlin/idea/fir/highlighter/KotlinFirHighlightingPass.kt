/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.fir.highlighter

import com.intellij.codeHighlighting.*
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import org.jetbrains.kotlin.idea.fir.highlighter.visitors.FirAfterResolveHighlightingVisitor
import org.jetbrains.kotlin.idea.frontend.api.analyze
import org.jetbrains.kotlin.idea.highlighter.AbstractHighlightingPassBase
import org.jetbrains.kotlin.idea.highlighter.HighlightInfoWrapper
import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.kotlin.psi.KtFile

class KotlinFirHighlightingPass(file: KtFile, document: Document) : AbstractHighlightingPassBase(file, document) {

    override val highlighter: Highlighter
        get() = KotlinFirHighlighter()

    class KotlinFirHighlighter : Highlighter {
        override fun highlight(element: PsiElement, highlightInfoWrapper: HighlightInfoWrapper) {
            if (element !is KtElement) return
            if (ApplicationManager.getApplication().isDispatchThread) {
                throw ProcessCanceledException()
            }
            analyze(element) {
                FirAfterResolveHighlightingVisitor
                    .createListOfVisitors(this, highlightInfoWrapper)
                    .forEach(element::accept)
            }
        }
    }

    class Factory : TextEditorHighlightingPassFactory {
        override fun createHighlightingPass(file: PsiFile, editor: Editor): TextEditorHighlightingPass? {
            if (file !is KtFile) return null
            return KotlinFirHighlightingPass(file, editor.document)
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

