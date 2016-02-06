/*
 * Copyright (c) 2013, Hidekatsu Hirose
 * Copyright (c) 2013, Hirose-Zouen
 * This file is subject to the terms and conditions defined in
 * This file is subject to the terms and conditions defined in
 * file 'LICENSE.txt', which is part of this source code package.
 */

package org.hirosezouen.hzutil

import java.io.BufferedReader
import java.io.File
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.io.OutputStream
import java.io.Reader
import java.io.Writer
import java.net.InetAddress
import java.nio.ByteBuffer
import java.util.Properties

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.io.Source
import scala.reflect.runtime.{universe => ru}
import scala.util.control.Exception._

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.filter.ThresholdFilter

import org.slf4j.Logger
import org.slf4j.LoggerFactory

object HZLog {
   /*
    * ch.qos.logback.classic.Level order
    * ALL < TRACE < DEBUG < INFO < WARN < ERROR < OFF
    */
    def getLogger(loggerName: String): Logger = LoggerFactory.getLogger(loggerName)
    def getLogger(cls: Class[_]): Logger = LoggerFactory.getLogger(cls)

    /* -------------------------------------------------------------------- */

    def rootLoggerLevelConcrete(level: Level) {
        val rootLogger = LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME).asInstanceOf[ch.qos.logback.classic.Logger]
        rootLogger.setLevel(level)
    }
    def rootLoggerLevel2Info()  = rootLoggerLevelConcrete(Level.INFO)
    def rootLoggerLevel2Error() = rootLoggerLevelConcrete(Level.ERROR)
    def rootLoggerLevel2Debug() = rootLoggerLevelConcrete(Level.DEBUG)
    def rootLoggerLevel2Trace() = rootLoggerLevelConcrete(Level.TRACE)

    /* -------------------------------------------------------------------- */

    def loggerLevelConcrete(level: Level, logger: Logger) {
        val logbackLogger = logger.asInstanceOf[ch.qos.logback.classic.Logger]
        logbackLogger.setLevel(level)
    }
    def loggerLevel2Info()(implicit logger: Logger)  = loggerLevelConcrete(Level.INFO,  logger)
    def loggerLevel2Error()(implicit logger: Logger) = loggerLevelConcrete(Level.ERROR, logger)
    def loggerLevel2Debug()(implicit logger: Logger) = loggerLevelConcrete(Level.DEBUG, logger)
    def loggerLevel2Trace()(implicit logger: Logger) = loggerLevelConcrete(Level.TRACE, logger)

    /* -------------------------------------------------------------------- */

    def rootAppenderFilterLevelConcrete(appenderName: String, filterName: String, level: Level) {
        val rootLogger = LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME).asInstanceOf[ch.qos.logback.classic.Logger]
        val appender = rootLogger.getAppender(appenderName)
        appender.getCopyOfAttachedFiltersList().foreach {
            case (f: ThresholdFilter) if(f.getName == filterName) => f.setLevel(level.toString)
            case _ => /* Nothing to do */
        }
    }
    def rootAppenderFilterLevel2Info(aName: String, fName: String)(implicit logger: Logger)  = rootAppenderFilterLevelConcrete(aName, fName, Level.INFO)
    def rootAppenderFilterLevel2Error(aName: String, fName: String)(implicit logger: Logger) = rootAppenderFilterLevelConcrete(aName, fName, Level.ERROR)
    def rootAppenderFilterLevel2Debug(aName: String, fName: String)(implicit logger: Logger) = rootAppenderFilterLevelConcrete(aName, fName, Level.DEBUG)
    def rootAppenderFilterLevel2Trace(aName: String, fName: String)(implicit logger: Logger) = rootAppenderFilterLevelConcrete(aName, fName, Level.TRACE)

    /* -------------------------------------------------------------------- */

    def appenderFilterLevelConcrete(appenderName: String, filterName: String, level: Level, logger: Logger) {
        val appender = logger.asInstanceOf[ch.qos.logback.classic.Logger].getAppender(appenderName)
        appender.getCopyOfAttachedFiltersList().foreach {
            case (f: ThresholdFilter) if(f.getName == filterName) => f.setLevel(level.toString)
            case _ => /* Nothing to do */
        }
    }
    def appenderFilterLevel2Info(aName: String, fName: String)(implicit logger: Logger)  = appenderFilterLevelConcrete(aName, fName, Level.INFO,  logger)
    def appenderFilterLevel2Error(aName: String, fName: String)(implicit logger: Logger) = appenderFilterLevelConcrete(aName, fName, Level.ERROR, logger)
    def appenderFilterLevel2Debug(aName: String, fName: String)(implicit logger: Logger) = appenderFilterLevelConcrete(aName, fName, Level.DEBUG, logger)
    def appenderFilterLevel2Trace(aName: String, fName: String)(implicit logger: Logger) = appenderFilterLevelConcrete(aName, fName, Level.TRACE, logger)

    /* -------------------------------------------------------------------- */

    private def checkAndlog(level: Level, f: (Logger) => Unit)(implicit logger: Logger) =
        if(level.isGreaterOrEqual(logger.asInstanceOf[ch.qos.logback.classic.Logger].getEffectiveLevel)) f(logger)

    def log_info(msg: => String)(implicit logger: Logger) = checkAndlog(Level.INFO, (l) => l.info(msg))
    def log_error(msg: => String)(implicit logger: Logger) = checkAndlog(Level.ERROR, (l) => l.error(msg))
    def log_error(exp: Throwable)(implicit logger: Logger) = checkAndlog(Level.ERROR, (l) => l.error("",exp))
    def log_error(msg: => String, exp: Throwable)(implicit logger: Logger) = checkAndlog(Level.ERROR, (l) => l.error(msg,exp))
    def log_debug(msg: => String)(implicit logger: Logger) = checkAndlog(Level.DEBUG, (l) => logger.debug(msg))
    def log_debug(msg: => String, exp: Throwable)(implicit logger: Logger) = checkAndlog(Level.DEBUG, (l) => l.debug(msg,exp))
    def log_trace(msg: => String)(implicit logger: Logger) = checkAndlog(Level.TRACE, (l) => logger.trace(msg))
    def log_trace(msg: => String, exp: Throwable)(implicit logger: Logger) = checkAndlog(Level.TRACE, (l) => logger.trace(msg,exp))

    def l_t(msg: => String = "")(implicit logger: Logger): Unit = {
        if(logger.asInstanceOf[ch.qos.logback.classic.Logger].getEffectiveLevel.isGreaterOrEqual(Level.TRACE)) {
            val ste = new Throwable().getStackTrace().apply(1)
            logger.trace("%s():%d:%s".format(ste.getMethodName,ste.getLineNumber,msg))
        }
    }
    case class HZLogTrace(tag:String)
    def l_tt(msg: => String = "")(implicit logger: Logger, trc: HZLogTrace): Unit = {
        if(logger.asInstanceOf[ch.qos.logback.classic.Logger].getEffectiveLevel.isGreaterOrEqual(Level.TRACE)) {
            val ste = new Throwable().getStackTrace().apply(1)
            logger.trace("%s:%s():%d:%s".format(trc.tag,ste.getMethodName,ste.getLineNumber,msg))
        }
    }
    def l_t2(msg: => String)(implicit logger: Logger) = log_trace(msg)(logger)
}

object HZIO {
    def using[A](r: InputStream)(f: InputStream => A): A = try {
        f(r)
    } finally {
        r.close()
    }

    def using[A](w: OutputStream)(f: OutputStream => Unit): Unit = try {
        f(w)
    } finally {
        w.close()
    }

    def using[A](r: Source)(f: Source => A): A = try {
        f(r)
    } finally {
        r.close()
    }

    def using[A](r: Reader)(f: Reader => A): A = try {
        f(r)
    } finally {
        r.close()
    }
 
    def using[A](w: Writer)(f: Writer => Unit): Unit = try {
        f(w)
    } finally {
        w.close()
    }
}

sealed abstract class HZPropertyConcrete(val fileName: String, val charset: String) {
    import HZIO._

    protected lazy val prop: Properties = new Properties()
    def loadProperty() = {
        if(charset.length ==0) using(new FileInputStream(fileName))(r => prop.load(r))
        else using(new InputStreamReader(new FileInputStream(fileName),charset))(r => prop.load(r))
    }
    def apply(key: String): Option[String] = {
        val value = prop.getProperty(key)
        if(value == null) None
        else Some(value)
    }
    def exists(key: String): Boolean = {
        prop.containsKey(key)
    }
    def nonEmpty(key: String): Boolean = {
        this(key) match {
            case Some("") => false
            case Some(_) => true
            case None => false
        }
    }
    def isEmpty(key: String): Boolean = !nonEmpty(key)
}

case class HZProperty(fn: String, cs: String = "") extends HZPropertyConcrete(fn,cs)

/**
 * inner class にアクセスするときは、T#Sのように記述する。
 * Enumerationの要素(列挙子)はinner class 'Value'なので、
 * このように記述する必要がある。
 */
case class HZConfig(fn: String, cs: String = "") extends HZPropertyConcrete(fn,cs) {
    /**
     * apply()メソッドで、prop(key)のように値を取得できる。
     */
    def apply[A](key: HZConfig.Key[A]): A = key()(this)
    def optString[A](key: HZConfig.Key[A]): Option[String] = this(key.path)
    def optVal[A](key: HZConfig.Key[A]): Option[A] = key.optVal()(this)
    def exists[A](key: HZConfig.Key[A]): Boolean = exists(key.path)
    def nonEmpty[A](key: HZConfig.Key[A]): Boolean = nonEmpty(key.path)
    def isEmpty[A](key: HZConfig.Key[A]): Boolean = isEmpty(key.path)
}

class StringNode[A <: StringNode[A]](value: String, op: String, parent: A) {
    self =>

    if(!(parent == null)) parent.addLeaf(self.asInstanceOf[A])

    var leafs = Set.empty[A]

    protected def addLeaf[B](l: B): B = {
        leafs += l.asInstanceOf[A]
        l
    }

    def isNode: Boolean = !leafs.isEmpty
    def isLeaf: Boolean = leafs.isEmpty

/*
    def leaf[B](v: String): B = {
        val constructor = m.erasure.getConstructor(classOf[String], classOf[String] ,m.erasure)
        val l = constructor.newInstance(v,self).asInstanceOf[A]
        leafs += l
        l.asInstanceOf[B]
    }
*/

    def path(): String = {
        val p = parent.path()
        if(p.length == 0)
            value
        else
            p + op + value
    }

    def paths(): List[String] = {
        paths((l: A) => true)
    }

    def paths(f: (A) => Boolean): List[String] = {
        def vals(node: A, nodes: List[String]): List[String] = {
            val ns = if(f(node)) node.path :: nodes
                     else nodes
            if(node.leafs.isEmpty)
                ns
            else
                ns ::: node.leafs.toList.map(l => vals(l,List.empty[String])).flatten
        }
        vals(self.asInstanceOf[A], List.empty[String])
    }
}

object HZConfig {

    trait Validator {
        def validate(op: Option[String], req: Boolean): Option[String] = op match {
            case Some(sv) => validate_concrete(sv)
            case None => if(req) Some("Required property not set") else None
        }
        def validate_concrete: (String) => Option[String]
    }

    object IntValidator extends Validator {
        def validate_concrete = (sv) =>
            catching(classOf[NumberFormatException]) opt sv.toInt match {
                case Some(_) => None
                case None => Some(s"NumberFormatException:$sv")
            }
    }

    object BooleanValidator extends Validator {
        def validate_concrete = (sv) =>
            sv.toLowerCase match {
                case "true" => None
                case "false" => None
                case s => Some(s"'$sv' is not Boolean")
            }
    }

    object FileValidator extends Validator {
        def validate_concrete = { sv =>
            val f = new File(sv)
            if(!f.exists)   Some(s"Not exitsts:$sv")
            else None
        }
    }

    object InetAddressValidator extends Validator {
        def validate_concrete = (sv) =>
            catching(classOf[IOException]) either InetAddress.getByName(sv) match {
                case Right(_) => None
                case Left(th: Throwable) => Some(th.getMessage)
            }
    }

    class Key[A](name: String, parent: Key[_])(implicit tA: ru.TypeTag[A]) extends StringNode[Key[_]](name,".",parent) {
        self =>

        var validator = new Validator {
            def validate_concrete: (String) => Option[String] = (_) => None
        }
        var required: Boolean = false

        def apply()(implicit conf: HZConfig): A = value()
        def optString()(implicit conf: HZConfig): Option[String] = conf(path())
        def optVal()(implicit conf: HZConfig): Option[A] = optString match {
            case Some(_) => Some(value())
            case None => None
        }
        def nonEmpty() (implicit conf: HZConfig): Boolean = conf.nonEmpty(this)
        def isEmpty() (implicit conf: HZConfig): Boolean = conf.isEmpty(this)

        private def getString(implicit conf: HZConfig): String = optString.getOrElse("")

        def value()(implicit conf: HZConfig): A = {
            val v = tA.tpe match {
                case t if(t == ru.typeTag[String].tpe)  => getString
                case t if(t == ru.typeTag[Int].tpe)     => getString.toInt
                case t if(t == ru.typeTag[Boolean].tpe) => getString.toBoolean
                case t if(t == ru.typeTag[File].tpe) => new File(getString)
                case t if(t == ru.typeTag[InetAddress].tpe) => InetAddress.getByName(getString)
                case t => throw new IllegalArgumentException("HZconfig.value:Unexpected type:%s".format(t))
            }

            v.asInstanceOf[A]
        }

        def leaf[B](n: String, req: Boolean = false, vf: Validator = null)(implicit tB: ru.TypeTag[B]): Key[B] = {
            val l = new Key[B](n,this)
            if(vf != null) l.validator = vf
            l.required = req
            l
        }

        def validate()(implicit conf: HZConfig): Option[String] = {
            val v = tA.tpe match {
                case t if(t == ru.typeTag[Int].tpe) => IntValidator
                case t if(t == ru.typeTag[Boolean].tpe) => BooleanValidator
                case t if(t == ru.typeTag[File].tpe) => FileValidator
                case t if(t == ru.typeTag[InetAddress].tpe) => InetAddressValidator
                case _ => validator
            }
            v.validate(optString, required)
        }

        def keies(): List[Key[_]] = keies((l: Key[_]) => true)
        def keies(f: (Key[_]) => Boolean): List[Key[_]] = {
//            leafs.filter(f).toList
            def vals(node: Key[_], nodes: List[Key[_]]): List[Key[_]] = {
                val ns = if(f(node)) node :: nodes
                         else nodes
                if(node.leafs.isEmpty)
                    ns
                else
                    ns ::: node.leafs.toList.map(l => vals(l,List.empty[Key[_]])).flatten
            }
            vals(self, List.empty[Key[_]])
        }

        override def toString(): String = path()
    }

    object RootKey extends Key("root",null.asInstanceOf[Key[String]]) {
        override def path(): String = {
            ""
        }
    }
}

object HZByteBufferUtil {
    def putByteToBuffer(p: Int, b: Byte)(implicit buffer: ByteBuffer) {
        buffer.put(p,b)
    }

    def getByteFromBuffer(p: Int)(implicit buffer: ByteBuffer):Byte = {
        buffer.get(p)
    }

    def putBytesToBuffer(start: Int, end: Int, bytes: Array[Byte])(implicit buffer: ByteBuffer) = {
        val len = end - start
        assert(len == bytes.length, f"putBytesToBuffer:expect=${len}%d,actual=${bytes.length}%d")
        buffer.clear
        buffer.position(start)
        buffer.put(bytes)
    }

    def getBytesFromBuffer(start: Int, end: Int)(implicit buffer: ByteBuffer): Array[Byte] = {
        val len = end - start
        val bytes = new Array[Byte](len)
        buffer.clear
        buffer.position(start)
        buffer.get(bytes)
        bytes
    }

    def replaceBuffer(bytes: Array[Byte])(implicit buffer: ByteBuffer) {
        assert(buffer.capacity == bytes.length, f"replaceBuffer:expect=${buffer.capacity}%d,actual=${bytes.length}%d")
        buffer.clear
        buffer.put(bytes)
    }

    def putBufferToBuffer(dStart: Int, dEnd: Int, sBuffer: ByteBuffer, _sStart: Int = -1, _sEnd: Int = -1)
                         (implicit dBuffer: ByteBuffer)
    {
        val sStart = if(_sStart == -1) sBuffer.position else _sStart
        val sEnd = if(_sEnd == -1) sBuffer.limit else _sEnd
        val dLen = dEnd - dStart
        val sLen = sEnd - sStart
        assert(dLen == sLen, f"putBufferToBuffer:expect=${dLen}%d,actual=${sLen}%d")

        dBuffer.clear
        dBuffer.position(dStart)

        sBuffer.clear
        sBuffer.position(sStart)
        sBuffer.limit(sEnd)

        dBuffer.put(sBuffer)
    }

    def getBufferFromBuffer(start: Int, end: Int)(implicit sBuffer: ByteBuffer): ByteBuffer = {
        assert(sBuffer != null, f"getBufferFromBuffer:sBuffer == null")

        sBuffer.clear
        val sLen = sBuffer.limit - sBuffer.position
        val dLen = end - start

        assert(start <= end, f"getBufferFromBuffer:${end} < ${start}")
        assert(end <= sBuffer.limit, f"getBufferFromBuffer:${sBuffer.limit} < ${end}")

        sBuffer.position(start)
        sBuffer.limit(end)
        
        sBuffer.slice
    }
}

/* vim: set expandtab: */
