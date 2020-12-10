/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.highlighter

import com.intellij.codeHighlighting.TextEditorHighlightingPass
import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.codeInsight.daemon.impl.UpdateHighlightersUtil
import com.intellij.openapi.editor.Document
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.openapi.project.DumbAware
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiRecursiveElementVisitor
import org.jetbrains.kotlin.psi.KtFile

abstract class AbstractHighlightingPassBase(
    protected val file: KtFile,
    document: Document
) : TextEditorHighlightingPass(file.project, document), DumbAware {

    protected val highlightInfoWrapper = HighlightInfoWrapper(file)
    private val cachedHighlighter by lazy { highlighter }

    protected abstract val highlighter: Highlighter

    fun highlightInfoCallback(callback: ((HighlightInfo) -> Unit)) {
        highlightInfoWrapper.callback = callback
    }

    fun resetHighlightInfoCallback() {
        highlightInfoWrapper.callback = null
    }

    override fun doCollectInformation(progress: ProgressIndicator) {
        highlightInfoWrapper.clear()
        runAnnotatorWithContext(file, highlightInfoWrapper)
    }

    protected open fun runAnnotatorWithContext(
        element: PsiElement,
        highlightInfoWrapper: HighlightInfoWrapper
    ) {
        element.accept(object : PsiRecursiveElementVisitor() {
            override fun visitElement(element: PsiElement) {
                cachedHighlighter.highlight(element, highlightInfoWrapper)
                super.visitElement(element)
            }
        })
    }

    override fun getInfos(): MutableList<HighlightInfo> = highlightInfoWrapper.infos()

    override fun doApplyInformationToEditor() {
        val highlightInfos = infos
        UpdateHighlightersUtil.setHighlightersToEditor(myProject, myDocument!!, 0, file.textLength, highlightInfos, colorsScheme, id)
    }

    interface Highlighter {
        fun highlight(element: PsiElement, highlightInfoWrapper: HighlightInfoWrapper)
    }

}