/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.highlighter

import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.codeInsight.daemon.impl.HighlightInfoType
import com.intellij.codeInsight.intention.EmptyIntentionAction
import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.codeInspection.ProblemHighlightType
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.util.TextRange
import com.intellij.util.containers.MultiMap
import com.intellij.xml.util.XmlStringUtil
import org.jetbrains.kotlin.diagnostics.Diagnostic
import org.jetbrains.kotlin.diagnostics.Severity
import org.jetbrains.kotlin.diagnostics.rendering.DefaultErrorMessages
import org.jetbrains.kotlin.idea.inspections.KotlinUniversalQuickFix
import org.jetbrains.kotlin.idea.util.application.isInternal
import org.jetbrains.kotlin.idea.util.application.isUnitTestMode

class HighlightPresentationInfo(
    val ranges: List<TextRange>,
    val nonDefaultMessage: String? = null,
    val highlightType: ProblemHighlightType? = null,
    val textAttributes: TextAttributesKey? = null
) {

    fun processDiagnostics(
        highlightInfoWrapper: HighlightInfoWrapper,
        diagnostics: List<Diagnostic>,
        infosByDiagnostic: MutableMap<Diagnostic, HighlightInfo>? = null,
        fixesMap: MultiMap<Diagnostic, IntentionAction>?
    ) {
        for (range in ranges) {
            for (diagnostic in diagnostics) {
                create(diagnostic, range) { info ->
                    infosByDiagnostic?.put(diagnostic, info)
                    fixesMap?.let { applyFixes(it, diagnostic, info) }
                    highlightInfoWrapper.add(info)
                }
            }
        }
    }

    private inline fun HighlightInfo.registerFix(intentionAction: IntentionAction) {
        this.registerFix(intentionAction, null, null, null, null)
    }

    internal fun applyFixes(
        fixesMap: MultiMap<Diagnostic, IntentionAction>,
        diagnostic: Diagnostic,
        highlightInfo: HighlightInfo
    ) {
        val fixes = fixesMap[diagnostic]
        fixes.forEach {
            when (it) {
                is KotlinUniversalQuickFix -> highlightInfo.registerFix(it)
                is IntentionAction -> highlightInfo.registerFix(it)
            }
        }

        if (diagnostic.severity == Severity.WARNING) {

            if (fixes.isEmpty()) {
                // if there are no quick fixes we need to register an EmptyIntentionAction to enable 'suppress' actions
                highlightInfo.registerFix(EmptyIntentionAction(diagnostic.factory.name!!))
            }
        }
    }

    private fun create(
        diagnostic: Diagnostic,
        range: TextRange,
        consumer: (HighlightInfo) -> Unit
    ) {
        val message = nonDefaultMessage ?: getDefaultMessage(diagnostic)
        val tooltip = getMessage(diagnostic)

        val highlightInfoType: HighlightInfoType =
            when (highlightType) {
                ProblemHighlightType.LIKE_UNUSED_SYMBOL -> HighlightInfoType.UNUSED_SYMBOL
                ProblemHighlightType.LIKE_UNKNOWN_SYMBOL -> HighlightInfoType.WRONG_REF
                ProblemHighlightType.LIKE_DEPRECATED -> HighlightInfoType.DEPRECATED
                ProblemHighlightType.LIKE_MARKED_FOR_REMOVAL -> HighlightInfoType.MARKED_FOR_REMOVAL
                else -> when (diagnostic.severity) {
                    Severity.ERROR -> HighlightInfoType.ERROR
                    Severity.WARNING -> if (highlightType == ProblemHighlightType.WEAK_WARNING) {
                        HighlightInfoType.WEAK_WARNING
                    } else HighlightInfoType.WARNING
                    Severity.INFO -> HighlightInfoType.WEAK_WARNING
                }
            }

        HighlightInfo.newHighlightInfo(highlightInfoType)
            .description(if (message.isNotEmpty()) message else tooltip)
            .range(range)
            .escapedToolTip(tooltip)
            .also { builder -> textAttributes?.let { builder.textAttributes(it) } }
            .also { builder ->
                if (diagnostic.severity == Severity.WARNING) {
                    builder.problemGroup(KotlinSuppressableWarningProblemGroup(diagnostic.factory))
                }
            }
            .createUnconditionally()
            .also { consumer(it) }
    }

    private fun getMessage(diagnostic: Diagnostic): String {
        var message = IdeErrorMessages.render(diagnostic)
        if (isInternal() || isUnitTestMode()) {
            val factoryName = diagnostic.factory.name
            message = if (message.startsWith("<html>")) {
                "<html>[$factoryName] ${message.substring("<html>".length)}"
            } else {
                "[$factoryName] $message"
            }
        }
        if (!message.startsWith("<html>")) {
            message = "<html><body>${XmlStringUtil.escapeString(message)}</body></html>"
        }
        return message
    }

    private fun getDefaultMessage(diagnostic: Diagnostic): String {
        val message = DefaultErrorMessages.render(diagnostic)
        return if (isInternal() || isUnitTestMode()) {
            "[${diagnostic.factory.name}] $message"
        } else {
            message
        }
    }

}
