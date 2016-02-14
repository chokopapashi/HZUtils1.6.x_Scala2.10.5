/*
 * Copyright (c) 2013, Hidekatsu Hirose
 * Copyright (c) 2013, Hirose-Zouen
 * This file is subject to the terms and conditions defined in
 * This file is subject to the terms and conditions defined in
 * file 'LICENSE.txt', which is part of this source code package.
 */

package org.hirosezouen

import java.nio.charset.Charset

import scala.collection.mutable
import scala.util.control.Exception._

package object hzutil {
    def arrayToString[A](array: mutable.Traversable[A], separator: String = ","): String = {
        val strBuilder = new StringBuilder()
        strBuilder ++= array.map(b => "%02X".format(b)).mkString(separator)
        strBuilder.mkString
    }

    def unsignedBigEndianShortBytes2Int(ab: Array[Byte]): Int = ((ab(0) << 8) & 0x0000ff00) | (ab(1) & 0x000000ff)
    def int2unsignedBigEndianShortBytes(i: Int): Array[Byte] = {
        val ab = new Array[Byte](2)
        ab(0) = ((i >> 8) & 0x000000ff).toByte
        ab(1) = (i & 0x000000ff).toByte
        ab
    }

    def unsignedBingEndianIntBytes2Long(ab: Array[Byte]): Long =
        ((ab(0) << 24) & 0x00000000ff000000L).toLong |
        ((ab(1) << 16) & 0x0000000000ff0000L).toLong |
        ((ab(2) <<  8) & 0x000000000000ff00L).toLong |
         (ab(3) & 0x00000000000000ffL).toLong

    def long2unsignedBigEndianIntBytes(l: Long): Array[Byte] = {
        val ab = new Array[Byte](4)
        ab(0) = ((l >> 24) & 0x00000000000000ffL).toByte
        ab(1) = ((l >> 16) & 0x00000000000000ffL).toByte
        ab(2) = ((l >>  8) & 0x00000000000000ffL).toByte
        ab(3) = (l & 0x00000000000000ffL).toByte
        ab
    }

    def hexDump(bytes: Array[Byte]): String = {
        var addr = 0
        val strBuilder = new StringBuilder
        def GRP_LEN = 0x10
        def GRP_STR_LEN = GRP_LEN * 2
        def isPrintableCharacter(i: Int): Boolean =
                Character.isDefined(i) && (Character.isLetterOrDigit(i) || !Character.isISOControl(i))
        for(bgrp <- bytes.grouped(GRP_LEN)) {
            val hex = bgrp.map(x => "%02x".format(x)).mkString.padTo(GRP_STR_LEN,' ')
            val str = bgrp.map(x => if(isPrintableCharacter(x)) "%c".format(x) else '?').mkString
            strBuilder.append("%08x : %s : %s%n".format(addr, hex, str))
            addr += GRP_LEN
        }
        
        strBuilder.toString
    }

    def string2ByteArray(src: String)(implicit cs: Charset = null): Array[Byte] = {
        var charset: Charset = cs
        if(charset == null)
            charset = Charset.forName(sys.props("file.encoding"))
        val buffer = mutable.ArrayBuffer.empty[Byte]
        var strBuilder = new StringBuilder()
        var itr = src.iterator
        var loopFlag = true
        while(loopFlag && itr.hasNext) {
            itr.next match {
                case '\\' if itr.hasNext => itr.next match {
                    case 'x' => {
                        val hexStr = itr.take(2).mkString
                        if(hexStr.length == 2) {
                            catching(classOf[NumberFormatException]) opt Integer.parseInt(hexStr,16) match {
                                case Some(i) => {
                                    buffer ++= strBuilder.mkString.getBytes(charset) /* There is to convert string to bytes */
                                    strBuilder = new StringBuilder()
                                    buffer += i.toByte
                                    itr = itr.drop(2)
                                }
                                case None => strBuilder += 'x' /* when it faild check hex string, re check next loop */
                            }
                        } else strBuilder ++= hexStr
                    }
                    case c => strBuilder += c
                }
                case '\\' => /* If the character is only '\', then it is skiped */
                case c    => strBuilder += c
            }
        }

        (buffer ++= strBuilder.mkString.getBytes(charset)).toArray
    }

    def hexStr2byteArray(src: String): Array[Byte] = {
        src.grouped(2).map(Integer.parseInt(_,16).toByte).toArray
    }
}

