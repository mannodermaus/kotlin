/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.highlighter

import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.codeInsight.daemon.impl.analysis.HighlightInfoHolder
import org.jetbrains.kotlin.psi.KtFile

class HighlightInfoWrapper(file: KtFile) {
    internal var callback: ((HighlightInfo) -> Unit)? = null
    private val holder = HighlightInfoHolder(file)
    private val infos = mutableListOf<HighlightInfo>()

    fun infos(): MutableList<HighlightInfo> {
        synchronized(infos) {
            return ArrayList(infos)
        }
    }

    @Suppress("UnstableApiUsage")
    fun add(info: HighlightInfo) {
        callback?.invoke(info)
        synchronized(infos) {
            infos.add(info)
        }
        holder.add(info)
        holder.queueToUpdateIncrementally()
    }

    fun clear() {
        synchronized(infos) {
            infos.clear()
        }
        holder.clear()
    }

}