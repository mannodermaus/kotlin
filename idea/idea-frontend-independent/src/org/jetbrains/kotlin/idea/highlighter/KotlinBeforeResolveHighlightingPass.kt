/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.highlighter

import com.intellij.codeHighlighting.*
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.extensions.ExtensionPointName
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import org.jetbrains.kotlin.psi.KtFile

class KotlinBeforeResolveHighlightingPass(file: KtFile, document: Document) : AbstractHighlightingPassBase(file, document) {

    override val highlighter: Highlighter
        get() = BeforeResolveHighlightingHighlighter(highlightInfoWrapper)

    private class BeforeResolveHighlightingHighlighter(private val highlightInfoWrapper: HighlightInfoWrapper) : Highlighter {
        private val visitor = BeforeResolveHighlightingVisitor(highlightInfoWrapper)
        private val extensions = EP_NAME.extensionList.map { it.createVisitor(highlightInfoWrapper) }

        override fun highlight(element: PsiElement, highlightInfoWrapper: HighlightInfoWrapper) {
            element.accept(visitor)
            extensions.forEach(element::accept)
        }
    }

    class Factory : TextEditorHighlightingPassFactory {
        override fun createHighlightingPass(file: PsiFile, editor: Editor): TextEditorHighlightingPass? {
            if (file !is KtFile) return null
            return KotlinBeforeResolveHighlightingPass(file, editor.document)
        }
    }

    class Registrar : TextEditorHighlightingPassFactoryRegistrar {
        override fun registerHighlightingPassFactory(registrar: TextEditorHighlightingPassRegistrar, project: Project) {
            registrar.registerTextEditorHighlightingPass(
                Factory(),
                TextEditorHighlightingPassRegistrar.Anchor.BEFORE,
                Pass.UPDATE_FOLDING,
                false,
                false
            )
        }
    }

    companion object {
        val EP_NAME = ExtensionPointName.create<BeforeResolveHighlightingExtension>("org.jetbrains.kotlin.beforeResolveHighlightingVisitor")
    }
}

interface BeforeResolveHighlightingExtension {
    fun createVisitor(highlightInfoWrapper: HighlightInfoWrapper): HighlightingVisitor
}