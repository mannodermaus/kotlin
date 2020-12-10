/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.fir.highlighter.visitors

import org.jetbrains.kotlin.idea.frontend.api.KtAnalysisSession
import org.jetbrains.kotlin.idea.highlighter.HighlightInfoWrapper
import org.jetbrains.kotlin.idea.highlighter.HighlightingVisitor

abstract class FirAfterResolveHighlightingVisitor(
    protected val analysisSession: KtAnalysisSession,
    protected val highlightInfoWrapper: HighlightInfoWrapper
) : HighlightingVisitor(highlightInfoWrapper) {

    companion object {
        fun createListOfVisitors(
            analysisSession: KtAnalysisSession,
            highlightInfoWrapper: HighlightInfoWrapper
        ): List<FirAfterResolveHighlightingVisitor> = listOf(
            TypeHighlightingVisitor(analysisSession, highlightInfoWrapper),
            FunctionCallHighlightingVisitor(analysisSession, highlightInfoWrapper),
            ExpressionsSmartcastHighlightingVisitor(analysisSession, highlightInfoWrapper),
            VariableReferenceHighlightingVisitor(analysisSession, highlightInfoWrapper),
        )
    }
}