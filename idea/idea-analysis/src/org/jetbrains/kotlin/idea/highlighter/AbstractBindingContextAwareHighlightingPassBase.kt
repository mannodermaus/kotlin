/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.highlighter

import com.intellij.openapi.editor.Document
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.openapi.progress.ProgressIndicator
import org.jetbrains.kotlin.idea.caches.resolve.analyzeWithAllCompilerChecks
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.resolve.BindingContext

@Suppress("UnstableApiUsage")
abstract class AbstractBindingContextAwareHighlightingPassBase(
    file: KtFile,
    document: Document
) : AbstractHighlightingPassBase(file, document) {

    protected var bindingContext: BindingContext? = null

    protected fun bindingContext(): BindingContext = bindingContext ?: error("bindingContext has to be acquired")

    protected open fun buildBindingContext(highlightInfoWrapper: HighlightInfoWrapper): BindingContext {
        val analysisResult = file.analyzeWithAllCompilerChecks()
        if (analysisResult.isError()) {
            throw ProcessCanceledException(analysisResult.error)
        }

        return analysisResult.bindingContext
    }

    override fun doCollectInformation(progress: ProgressIndicator) {
        highlightInfoWrapper.clear()
        bindingContext = buildBindingContext(highlightInfoWrapper)
        runAnnotatorWithContext(file, highlightInfoWrapper)
    }

}