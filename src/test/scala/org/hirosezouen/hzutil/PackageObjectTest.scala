package org.hirosezouen.hzutil

import org.scalatest.FunSuite

import org.hirosezouen.hzutil._

class PackageObjectTest extends FunSuite {
    test("unsignedBigEndianShortBytes2Int") {
        assertResult(0x0000ABCD.toInt)(unsignedBigEndianShortBytes2Int(Array[Byte](0xAB.toByte,0xCD.toByte)))
        assertResult(0x0000FE01.toInt)(unsignedBigEndianShortBytes2Int(Array[Byte](0xFE.toByte,0x01.toByte)))
    }

    test("int2unsignedBigEndianShortBytes") {
        assert(Array[Byte](0xAB.toByte,0xCD.toByte) sameElements int2unsignedBigEndianShortBytes(0x0000ABCD.toInt))
        assert(Array[Byte](0xFE.toByte,0x01.toByte) sameElements int2unsignedBigEndianShortBytes(0x0000FE01.toInt))
    }

    test("unsignedBingEndianIntBytes2Long") {
        assertResult(0x00000000ABCDEF12L)(unsignedBingEndianIntBytes2Long(Array[Byte](0xAB.toByte,0xCD.toByte,0xEF.toByte,0x12.toByte)))
        assertResult(0x00000000FEDCBA98L)(unsignedBingEndianIntBytes2Long(Array[Byte](0xFE.toByte,0xDC.toByte,0xBA.toByte,0x98.toByte)))
    }

    test("long2unsignedBigEndianIntBytes") {
        assert(Array[Byte](0xAB.toByte,0xCD.toByte,0xEF.toByte,0x12.toByte) sameElements long2unsignedBigEndianIntBytes(0x00000000ABCDEF12L.toInt))
        assert(Array[Byte](0xFE.toByte,0xDC.toByte,0xBA.toByte,0x98.toByte) sameElements long2unsignedBigEndianIntBytes(0x00000000FEDCBA98L.toInt))
    }

    test("hexDump") {

        val e = f"00000000 : 30313233343536373839414243444546 : 0123456789ABCDEF%n" +
                f"00000010 : 303132333435363738390d0a41424344 : 0123456789??ABCD%n" +
                f"00000020 : 30313233343536373839             : 0123456789%n"
        val a = Array[Byte](0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x41,0x42,0x43,0x44,0x45,0x46,
                            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x0d,0x0a,0x41,0x42,0x43,0x44,
                            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39)
/*
        println(e)
        println(e.length)
        println(e.map(c => c.toInt).map(c => f"$c%02x").mkString(","))
        val h = hexDump(a)
        println(h)
        println(h.length)
        println(h.map(c => c.toInt).map(c => f"$c%02x").mkString(","))
*/
        assertResult(e)(hexDump(a))
    }

    test("string2ByteArray_1") {
        val e1 = Array[Byte](32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
                             51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
                             70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88,
                             89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
                             107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122,
                             123, 124, 125, 126)
        import java.nio.charset.Charset
        implicit val cs = Charset.forName("Shift_JIS")
        val a1 = string2ByteArray(""" !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~""")
//        println(a1.map(s =>f"$s%02X").mkString)
        assert(e1 sameElements a1)
    }

    test("string2ByteArray_2") {
        val e1 = Array(0x82, 0xA0, 0x82, 0xA2, 0x82, 0xA4, 0x82, 0xA6, 0x82, 0xA8, 0x94, 0x5C,
                      0x00, 0x01, 0x02, 0xab, 0xcd, 0xef, 0xff, 0x78, 0x7A, 0x7A, 0x61, 0x78, 0x61).map(_.toByte)
        import java.nio.charset.Charset
        val a1 = string2ByteArray("""あいうえお能\x00\x01\x02\xab\xcd\xef\xff\xzz\a\x\a""")(Charset.forName("Shift_JIS"))
//        println(a1.map(s => f"$s%02X").mkString)
        assert(e1 sameElements a1)
    }
    
    test("hexStr2byteArray") {
        val e1 = "000102030405060708090a0b0c0d0e0ff0e1d2c3b4a5968778695a4b3c2d1e0f"
        val a1 = Array(0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f,
                       0xf0,0xe1,0xd2,0xc3,0xb4,0xa5,0x96,0x87,0x78,0x69,0x5a,0x4b,0x3c,0x2d,0x1e,0x0f).map(_.toByte)
        assert(hexStr2byteArray(e1) sameElements a1)
    }
}

