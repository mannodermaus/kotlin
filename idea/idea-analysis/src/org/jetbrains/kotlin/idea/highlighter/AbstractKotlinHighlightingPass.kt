/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.highlighter

import com.intellij.codeHighlighting.TextEditorHighlightingPass
import com.intellij.codeInsight.daemon.impl.AnnotationHolderImpl
import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.codeInsight.daemon.impl.UpdateHighlightersUtil
import com.intellij.lang.annotation.AnnotationHolder
import com.intellij.lang.annotation.AnnotationSession
import com.intellij.lang.annotation.Annotator
import com.intellij.openapi.editor.Document
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.openapi.project.DumbAware
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiRecursiveElementVisitor
import org.jetbrains.kotlin.idea.caches.resolve.analyzeWithAllCompilerChecks
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.resolve.BindingContext

@Suppress("UnstableApiUsage")
abstract class AbstractKotlinHighlightingPass(
    protected val file: KtFile,
    document: Document,
    private val enableAnalyze: Boolean = true
) : TextEditorHighlightingPass(file.project, document), DumbAware {

    @Volatile
    private var annotationHolder: AnnotationHolderImpl? = null
    private val highlightInfos: MutableList<HighlightInfo> = mutableListOf()
    private val cachedAnnotator by lazy { annotator }

    protected var bindingContext: BindingContext? = null
    protected abstract val annotator: Annotator

    protected fun bindingContext(): BindingContext {
        return if (enableAnalyze) {
            bindingContext ?: error("bindingContext has to be acquired")
        } else {
            error("bindingContext is not enabled for ${this::javaClass.name}")
        }
    }

    override fun doCollectInformation(progress: ProgressIndicator) {
        highlightInfos.clear()
        bindingContext = if (enableAnalyze) {
            val analysisResult = file.analyzeWithAllCompilerChecks()
            if (analysisResult.isError()) {
                throw ProcessCanceledException(analysisResult.error)
            }

            analysisResult.bindingContext
        } else {
            null
        }

        val annotationHolder = AnnotationHolderImpl(AnnotationSession(file))
        try {
            annotationHolder.runAnnotatorWithContext(file) { element, holder ->
                runAnnotatorWithContext(element, holder)
            }
            this.annotationHolder = annotationHolder
        } finally {
            bindingContext = null
        }
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