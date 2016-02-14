package org.hirosezouen.hzutil

import java.io.ByteArrayOutputStream

import java.nio.ByteBuffer

import org.scalatest.FunSuite

import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.filter.ThresholdFilter
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.filter.Filter
import ch.qos.logback.core.encoder.Encoder
import ch.qos.logback.core.OutputStreamAppender

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import org.hirosezouen.hzutil.HZLog._
import org.hirosezouen.hzutil.HZByteBufferUtil._

class HZUtilTest extends FunSuite {

    case class TestLogger(name: String, levelOpt: Option[Level], filterLevelOpt: Option[Level] = None) {

        val byteOutStream = new ByteArrayOutputStream
        val rootByteOutStream = new ByteArrayOutputStream
        val logger = getLogger("test")   
        val rootLogger = LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME).asInstanceOf[ch.qos.logback.classic.Logger]

        def newEncoder(context: LoggerContext) = {
            val encoder = new PatternLayoutEncoder();
            encoder.setContext(context)
            encoder.setPattern("%-5level - %message%n")
            encoder.start()
            encoder
        }

        def newAppender(context: LoggerContext, encoder: PatternLayoutEncoder, baos: ByteArrayOutputStream) = {
            val appender = new OutputStreamAppender[ILoggingEvent]
            appender.setName("TestAppender")
            appender.setContext(context)
            appender.setEncoder(encoder)
            appender.setOutputStream(baos)
            filterLevelOpt match {
                case Some(level) => {
                    val filter = new ThresholdFilter()
                    filter.setName("TestFilter")
                    filter.setLevel(level.toString)
                    filter.start
                    appender.addFilter(filter)
                }
                case _ => /* Nothing to do. */
            }
            appender.start
            appender
        }

        {
            val loggerContext = rootLogger.getLoggerContext()
            loggerContext.reset 

            {
                val appender = newAppender(loggerContext, newEncoder(loggerContext), rootByteOutStream)
                rootLogger.addAppender(appender)
                levelOpt match {
                    case Some(level) => rootLogger.setLevel(level)
                    case None        => /* Nothing to do. */
                }
            }

            {
                val appender = newAppender(loggerContext, newEncoder(loggerContext), byteOutStream)
                val logbackLogger = logger.asInstanceOf[ch.qos.logback.classic.Logger]
                logbackLogger.addAppender(appender)
                levelOpt match {
                    case Some(level) => logbackLogger.setLevel(level)
                    case None        => /* Nothing to do. */
                }
            }
        }

        def getLog: String = new String(byteOutStream.toByteArray)
        def getRootLog: String = new String(rootByteOutStream.toByteArray)
        def resetLog = {
            byteOutStream.reset
            rootByteOutStream.reset
        }
    }

    test("HZLog.rootLoggerLevel2xxx().01") {
        val test_logger = TestLogger("HZLog.rootLoggerLevel2xxx().01", None)
        implicit val logger = test_logger.logger

        rootLoggerLevel2Trace()
        log_trace("Levele.TRACE + " + "log_trace()")
        assertResult(f"TRACE - Levele.TRACE + log_trace()%n")(test_logger.getRootLog)

        test_logger.resetLog
        rootLoggerLevel2Debug()
        log_trace("Levele.DEBUG + " + "log_trace()")
        assertResult("")(test_logger.getRootLog)

        test_logger.resetLog
        log_debug("Levele.DEBUG + " + "log_debug()")
        assertResult(f"DEBUG - Levele.DEBUG + log_debug()%n")(test_logger.getRootLog)

        test_logger.resetLog
        rootLoggerLevel2Info()
        log_debug("Levele.INFO + " + "log_debug()")
        assertResult("")(test_logger.getRootLog)

        test_logger.resetLog
        log_info("Levele.INFO + " + "log_info()")
        assertResult(f"INFO  - Levele.INFO + log_info()%n")(test_logger.getRootLog)

        test_logger.resetLog
        rootLoggerLevel2Error()
        log_debug("Levele.Error + " + "log_debug()")
        assertResult("")(test_logger.getRootLog)
    }

    test("HZLog.loggerLevel2xxx().01") {
        val test_logger = TestLogger("HZLog.loggerLevel2xxx().01", Some(Level.TRACE))
        implicit val logger = test_logger.logger

        loggerLevel2Trace()
        log_trace("Levele.TRACE + " + "log_trace()")
        assertResult(f"TRACE - Levele.TRACE + log_trace()%n")(test_logger.getLog)

        test_logger.resetLog
        loggerLevel2Debug()
        log_trace("Levele.DEBUG + " + "log_trace()")
        assertResult("")(test_logger.getLog)

        test_logger.resetLog
        log_debug("Levele.DEBUG + " + "log_debug()")
        assertResult(f"DEBUG - Levele.DEBUG + log_debug()%n")(test_logger.getLog)

        test_logger.resetLog
        loggerLevel2Info()
        log_debug("Levele.INFO + " + "log_debug()")
        assertResult("")(test_logger.getLog)

        test_logger.resetLog
        log_info("Levele.INFO + " + "log_info()")
        assertResult(f"INFO  - Levele.INFO + log_info()%n")(test_logger.getLog)

        test_logger.resetLog
        loggerLevel2Error()
        log_debug("Levele.Error + " + "log_debug()")
        assertResult("")(test_logger.getLog)
    }

    test("HZLog.rootAppenderFilterLevel2xxx().01") {
        val test_logger = TestLogger("HZLog.rootAppenderFilterLevel2xxx().01", Some(Level.TRACE), Some(Level.INFO))
        implicit val logger = test_logger.logger

        rootAppenderFilterLevel2Trace("TestAppender", "TestFilter")
        log_trace("Levele.TRACE + " + "log_trace()")
        assertResult(f"TRACE - Levele.TRACE + log_trace()%n")(test_logger.getRootLog)

        test_logger.resetLog
        rootAppenderFilterLevel2Debug("TestAppender", "TestFilter")
        log_trace("Levele.DEBUG + " + "log_trace()")
        assertResult("")(test_logger.getRootLog)

        test_logger.resetLog
        log_debug("Levele.DEBUG + " + "log_debug()")
        assertResult(f"DEBUG - Levele.DEBUG + log_debug()%n")(test_logger.getRootLog)

        test_logger.resetLog
        rootAppenderFilterLevel2Info("TestAppender", "TestFilter")
        log_debug("Levele.INFO + " + "log_debug()")
        assertResult("")(test_logger.getRootLog)

        test_logger.resetLog
        log_info("Levele.INFO + " + "log_info()")
        assertResult(f"INFO  - Levele.INFO + log_info()%n")(test_logger.getRootLog)

        test_logger.resetLog
        rootAppenderFilterLevel2Error("TestAppender", "TestFilter")
        log_debug("Levele.Error + " + "log_debug()")
        assertResult("")(test_logger.getRootLog)
    }

    test("HZLog.appenderFilterLevel2xxx().01") {
        val test_logger = TestLogger("HZLog.appenderFilterLevel2xxx().01", Some(Level.TRACE), Some(Level.INFO))
        implicit val logger = test_logger.logger

        appenderFilterLevel2Trace("TestAppender", "TestFilter")
        log_trace("Levele.TRACE + " + "log_trace()")
        assertResult(f"TRACE - Levele.TRACE + log_trace()%n")(test_logger.getLog)

        test_logger.resetLog
        appenderFilterLevel2Debug("TestAppender", "TestFilter")
        log_trace("Levele.DEBUG + " + "log_trace()")
        assertResult("")(test_logger.getLog)

        test_logger.resetLog
        log_debug("Levele.DEBUG + " + "log_debug()")
        assertResult(f"DEBUG - Levele.DEBUG + log_debug()%n")(test_logger.getLog)

        test_logger.resetLog
        appenderFilterLevel2Info("TestAppender", "TestFilter")
        log_debug("Levele.INFO + " + "log_debug()")
        assertResult("")(test_logger.getLog)

        test_logger.resetLog
        log_info("Levele.INFO + " + "log_info()")
        assertResult(f"INFO  - Levele.INFO + log_info()%n")(test_logger.getLog)

        test_logger.resetLog
        appenderFilterLevel2Error("TestAppender", "TestFilter")
        log_debug("Levele.Error + " + "log_debug()")
        assertResult("")(test_logger.getLog)
    }

    test("HZLog.log_info(msg).01") {
        val test_logger = TestLogger("HZLog.log_info(msg).01", Some(Level.INFO))
        implicit val logger = test_logger.logger

        log_info("this is " + "test log_info(msg)" + " 01")
//        println("'%s'".format(test_logger.getLog))
//        test_logger.byteOutStream.toByteArray.foreach(printf("%02X",_))
        assertResult(f"INFO  - this is test log_info(msg) 01%n")(test_logger.getLog)
    }

    test("HZLog.log_debug(msg).01") {
        val test_logger = TestLogger("HZLog.log_debug(msg).01", Some(Level.INFO))
        implicit val logger = test_logger.logger

        var debug_count = 0
        def f(): String = {
            debug_count += 1
            debug_count.toString
        }

        log_debug("this is " + "test log_debug(" + f() +") 01")
        assertResult("")(test_logger.getLog)
        assertResult(0)(debug_count)
    }

    test("HZLog.l_t(msg).01") {
        val test_logger = TestLogger("HZLog.l_t(msg).01", Some(Level.INFO))
        implicit val logger = test_logger.logger

        l_t("this is " + "test l_t(msg) 01")
        assertResult("")(test_logger.getLog)
    }

    test("HZLog.l_t(msg).02") {
        val test_logger = TestLogger("HZLog.l_t(msg).02", Some(Level.TRACE))
        implicit val logger = test_logger.logger

        val ste = new Throwable().getStackTrace().apply(0)
        l_t("this is " + "test l_t(msg) 02")

//        printf("'%d'%n",ste.getLineNumber)
//        printf("'%s'%n",test_logger.getLog)
        assert(test_logger.getLog.matches(f"""TRACE - .*:${ste.getLineNumber+1}:this is test l_t\\(msg\\) 02%n"""))
    }

    test("HZProperty.01") {
        val testdata = sys.props("user.dir") + "/src/test/resources/test_data/hzProperty_test.properties"
        val prop = HZProperty(testdata)
        prop.loadProperty()

        assertResult(prop("TestKey.a"))(Some("123"))
        assertResult(prop("TestKey.b"))(Some("456"))
        assertResult(prop("TestKey.c.d"))(Some("abc"))
        assertResult(prop("TestKey.c.e"))(Some("def"))
        assertResult(prop("TestKey.c.f.g"))(Some("true"))
        assertResult(prop("TestKey.c.f.h"))(Some("false"))
        assertResult(prop("TestKey.c.f.i"))(Some(""))

        assert(prop.exists("TestKey.a"))
        assert(prop.exists("TestKey.b"))
        assert(prop.exists("TestKey.c.d"))
        assert(prop.exists("TestKey.c.e"))
        assert(prop.exists("TestKey.c.f.g"))
        assert(prop.exists("TestKey.c.f.h"))
        assert(prop.exists("TestKey.c.f.i"))
        assert(!prop.exists("TestKey.x.x.x"))

        assert(prop.nonEmpty("TestKey.a"))
        assert(prop.nonEmpty("TestKey.b"))
        assert(prop.nonEmpty("TestKey.c.d"))
        assert(prop.nonEmpty("TestKey.c.e"))
        assert(prop.nonEmpty("TestKey.c.f.g"))
        assert(prop.nonEmpty("TestKey.c.f.h"))
        assert(prop.isEmpty("TestKey.c.f.i"))
        assert(prop.isEmpty("TestKey.x.x.x"))
    }

    test("HZConfig.01") {
        import HZConfig._

        object TestKey extends Key("TestKey",RootKey) {

            val a: Key[Int] = leaf("a")
            val b: Key[Int] = leaf("b")
            val c = new C("c", this)
            class C(cn: String, cp: Key[_]) extends Key(cn,cp) {
                val d: Key[String] = leaf("d")
                val e: Key[String] = leaf("e")
                val f = new F("f",this)
                class F(fn: String, fp: Key[_]) extends Key(fn,fp) {
                    val g = leaf[Boolean]("g")
                    val h = leaf[Boolean]("h")
                    val i = leaf[String]("i")
                }
            }
        }

        val testdata = sys.props("user.dir") + "/src/test/resources/test_data/hzProperty_test.properties"
        implicit val conf = HZConfig(testdata)
        conf.loadProperty()
        assertResult(TestKey.a.optString())(Some("123"))
        assertResult(TestKey.b.optString())(Some("456"))
        assertResult(TestKey.c.d.optString())(Some("abc"))
        assertResult(TestKey.c.e.optString())(Some("def"))
        assertResult(TestKey.c.f.g.optString())(Some("true"))
        assertResult(TestKey.c.f.h.optString())(Some("false"))
        assertResult(TestKey.c.f.i.optString())(Some(""))

        assertResult(TestKey.a.optVal())(Some(123))
        assertResult(TestKey.b.optVal())(Some(456))
        assertResult(TestKey.c.d.optVal())(Some("abc"))
        assertResult(TestKey.c.e.optVal())(Some("def"))
        assertResult(TestKey.c.f.g.optVal())(Some(true))
        assertResult(TestKey.c.f.h.optVal())(Some(false))
        assertResult(TestKey.c.f.i.optVal())(Some(""))

        assertResult(TestKey.a())(123)
        assertResult(TestKey.b())(456)
        assertResult(TestKey.c.d())("abc")
        assertResult(TestKey.c.e())("def")
        assertResult(TestKey.c.f.g())(true)
        assertResult(TestKey.c.f.h())(false)
        assertResult(TestKey.c.f.i())("")

        assert(TestKey.a.nonEmpty())
        assert(TestKey.b.nonEmpty())
        assert(TestKey.c.d.nonEmpty())
        assert(TestKey.c.e.nonEmpty())
        assert(TestKey.c.f.g.nonEmpty())
        assert(TestKey.c.f.h.nonEmpty())
        assert(TestKey.c.f.i.isEmpty())

        assertResult(TestKey.paths)(List("TestKey","TestKey.a","TestKey.b","TestKey.c","TestKey.c.d","TestKey.c.e","TestKey.c.f","TestKey.c.f.g","TestKey.c.f.h","TestKey.c.f.i"))
        assertResult(TestKey.paths((l: Key[_]) => l.isLeaf))(List("TestKey.a","TestKey.b","TestKey.c.d","TestKey.c.e","TestKey.c.f.g","TestKey.c.f.h","TestKey.c.f.i"))

        assert(TestKey.keies((l: Key[_]) => l.isLeaf).forall(_.isInstanceOf[Key[_]]))
        assertResult(TestKey.keies((l: Key[_]) => l.isLeaf).map(_.toString))(List("TestKey.a","TestKey.b","TestKey.c.d","TestKey.c.e","TestKey.c.f.g","TestKey.c.f.h","TestKey.c.f.i"))
    }

    test("HZByteBufferUtil:sputByteToBuffer") {
        val data = new Array[Byte](6)
        implicit val buffer = ByteBuffer.wrap(data)
        putByteToBuffer(2,0xAB.toByte)

        assertResult(0xAB.toByte)(buffer.get(2))
    }

    test("HZByteBufferUtil:getByteFromBuffer") {
        val data = new Array[Byte](6)
        implicit val buffer = ByteBuffer.wrap(data)
        buffer.put(3,0xAB.toByte)
        assertResult(0xAB.toByte)(getByteFromBuffer(3))
    }

    test("HZByteBufferUtil:putBytesToBuffer") {
        val data = new Array[Byte](6)
        implicit val buffer = ByteBuffer.wrap(data)
        putBytesToBuffer(2,5,Array(0xAB,0xCD,0xEF).map(_.toByte))
    
        assertResult(0xAB.toByte)(buffer.get(2))
        assertResult(0xCD.toByte)(buffer.get(3))
        assertResult(0xEF.toByte)(buffer.get(4))
    }

    test("HZByteBufferUtil:getBytesFromBuffer") {
        val data = new Array[Byte](6)
        implicit val buffer = ByteBuffer.wrap(data)
        buffer.position(3)
        buffer.put(Array(0xAB,0xCD,0xEF).map(_.toByte))
        
        val outData = getBytesFromBuffer(3,6)
        assertResult(0xAB.toByte)(outData(0))
        assertResult(0xCD.toByte)(outData(1))
        assertResult(0xEF.toByte)(outData(2))
    }

    test("HZByteBufferUtil:replaceBuffer") {
        val data = new Array[Byte](6)
        implicit val buffer = ByteBuffer.wrap(data)

        replaceBuffer(Array(0xAB,0xCD,0xEF,0x01,0x02,0x03).map(_.toByte))

        assertResult(0xAB.toByte)(data(0))
        assertResult(0xCD.toByte)(data(1))
        assertResult(0xEF.toByte)(data(2))
        assertResult(0x01.toByte)(data(3))
        assertResult(0x02.toByte)(data(4))
        assertResult(0x03.toByte)(data(5))
    }

    test("HZByteBufferUtil:putBufferToBuffer_01") {
        val data = new Array[Byte](6)
        implicit val dBuffer = ByteBuffer.wrap(data)

        val srcData = new Array[Byte](6)
        val srcBuffer = ByteBuffer.wrap(srcData)
        srcBuffer.position(2)
        srcBuffer.put(Array(0xAB,0xCD,0xEF).map(_.toByte))
        srcBuffer.flip
        srcBuffer.position(2)

        putBufferToBuffer(2,5,srcBuffer,2,5)

//        print(s"${arrayToString(data)}%n")
        assertResult(0x00.toByte)(data(0))
        assertResult(0x00.toByte)(data(1))
        assertResult(0xAB.toByte)(data(2))
        assertResult(0xCD.toByte)(data(3))
        assertResult(0xEF.toByte)(data(4))
        assertResult(0x00.toByte)(data(5))
    }

    test("HZByteBufferUtil:putBufferToBuffer_02") {
        val data = new Array[Byte](6)
        implicit val dBuffer = ByteBuffer.wrap(data)

        val srcData = Array(0xAB,0xCD,0xEF).map(_.toByte)
        val srcBuffer = ByteBuffer.wrap(srcData)
        srcBuffer.clear

        putBufferToBuffer(2,5,srcBuffer)

//        print(s"${arrayToString(data)}%n")
        assertResult(0x00.toByte)(data(0))
        assertResult(0x00.toByte)(data(1))
        assertResult(0xAB.toByte)(data(2))
        assertResult(0xCD.toByte)(data(3))
        assertResult(0xEF.toByte)(data(4))
        assertResult(0x00.toByte)(data(5))
    }

    test("HZByteBufferUtil:getBufferFromBuffer") {
        val data =  Array(0xAB,0xCD,0xEF,0x01,0x02,0x03).map(_.toByte)
        implicit val buffer = ByteBuffer.wrap(data)

        val dBuffer = getBufferFromBuffer(2,5)
        dBuffer.clear
        assertResult(3)(dBuffer.limit)
        assertResult(3)(dBuffer.capacity)

        assertResult(0xEF.toByte)(dBuffer.get(0))
        assertResult(0x01.toByte)(dBuffer.get(1))
        assertResult(0x02.toByte)(dBuffer.get(2))
    }
}

