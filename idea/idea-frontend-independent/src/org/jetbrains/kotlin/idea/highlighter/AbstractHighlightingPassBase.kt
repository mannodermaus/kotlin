/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.highlighter

import com.intellij.codeHighlighting.TextEditorHighlightingPass
import com.intellij.codeInsight.daemon.impl.AnnotationHolderImpl
import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.codeInsight.daemon.impl.UpdateHighlightersUtil
import com.intellij.lang.annotation.Annotation
import com.intellij.lang.annotation.AnnotationHolder
import com.intellij.lang.annotation.AnnotationSession
import com.intellij.lang.annotation.Annotator
import com.intellij.openapi.editor.Document
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.openapi.project.DumbAware
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiRecursiveElementVisitor
import org.jetbrains.kotlin.psi.KtFile

@Suppress("UnstableApiUsage")
abstract class AbstractHighlightingPassBase(
    protected val file: KtFile,
    document: Document,
    private val enableAnalyze: Boolean = true
) : TextEditorHighlightingPass(file.project, document), DumbAware {

    @Volatile
    protected var annotationCallback: ((Annotation) -> Unit)? = null
    @Volatile
    protected var annotationHolder: AnnotationHolderImpl? = null
    private val highlightInfos: MutableList<HighlightInfo> = mutableListOf()
    private val cachedAnnotator by lazy { annotator }
    protected abstract val annotator: Annotator

    fun annotationCallback(callback: (Annotation) -> Unit) {
        annotationCallback = callback
    }

    fun resetAnnotationCallback() {
        annotationCallback = null
    }

    override fun doCollectInformation(progress: ProgressIndicator) {
        highlightInfos.clear()

        val annotationHolder = object: AnnotationHolderImpl(AnnotationSession(file)) {
            override fun add(element: Annotation?): Boolean {
                element?.let { annotationCallback?.invoke(it) }
                return super.add(element)
            }
        }
        annotationHolder.runAnnotatorWithContext(file) { element, holder ->
            runAnnotatorWithContext(element, holder)
        }
        this.annotationHolder = annotationHolder
    }

    protected open fun runAnnotatorWithContext(
        element: PsiElement,
        holder: AnnotationHolder
    ) {
        element.accept(object : PsiRecursiveElementVisitor() {
            override fun visitElement(element: PsiElement) {
                cachedAnnotator.annotate(element, holder)
                super.visitElement(element)
            }
        })
    }

    override fun getInfos(): MutableList<HighlightInfo> = highlightInfos

    override fun doApplyInformationToEditor() {
        try {
            val infos = annotationHolder?.map { HighlightInfo.fromAnnotation(it) } ?: return
            highlightInfos.addAll(infos)
            UpdateHighlightersUtil.setHighlightersToEditor(myProject, myDocument, 0, file.textLength, infos, colorsScheme, id)
        } finally {
            annotationHolder = null
        }
    }

}