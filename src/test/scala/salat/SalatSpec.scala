package com.bumnetworks.salat.test

import com.bumnetworks.salat._
import com.bumnetworks.salat.global._
import com.bumnetworks.salat.test.model._
import com.mongodb.casbah.Imports._

import scala.collection.mutable.{Buffer, ArrayBuffer}
import scala.tools.scalap.scalax.rules.scalasig._
import scala.math.{BigDecimal => ScalaBigDecimal}

import org.specs._
import org.specs.specification.PendingUntilFixed

object SalatSpec extends Specification with PendingUntilFixed with CasbahLogging {
  detailedDiffs()

  doBeforeSpec {
    com.mongodb.casbah.commons.conversions.scala.RegisterConversionHelpers()
  }

  "a grater" should {
    "make DBObject-s out of case class instances" in {
      "properly treat primitive values and optional values" in {
        val e = numbers
        val dbo: MongoDBObject = grater[E].asDBObject(e)

        log.info("before: %s", e)
        log.info("after : %s", dbo.asDBObject)

        dbo must havePair("a" -> e.a)
        dbo must notHaveKey("aa")
        dbo must havePair("aaa" -> e.aaa.get)

        dbo must havePair("b" -> e.b)
        dbo must notHaveKey("bb")
        dbo must havePair("bbb" -> e.bbb.get)

        dbo must havePair("c" -> e.c)
        dbo must notHaveKey("cc")
        dbo must havePair("ccc" -> e.ccc.get)
      }

      "work with object graphs" in {
        val a = graph
        val dbo: MongoDBObject = grater[A].asDBObject(a)
        log.info("before: %s", a)
        log.info("after : %s", dbo.asDBObject)
        dbo must havePair("x" -> "x")
      }
    }

    "instantiate case class instances using data from DBObject-s" in {
      "cover primitive types" in {
        val e = numbers
        val e_* = grater[E].asObject(grater[E].asDBObject(e))
        e_* must_== e
      }

      "and silly object graphs" in {
        val a = graph
        val a_* = grater[A].asObject(grater[A].asDBObject(a))
        // these two checks are *very* naive, but it's hard to compare
        // unordered maps and expect them to come out equal.
        a_*.z.p must_== a.z.p
        a_*.z.q must_== a.z.q
      }

      "and also object graphs of even sillier shapes" in {
        val f = mucho_numbers()
        val dbo: MongoDBObject = grater[F].asDBObject(f)
        dbo.get("complicated") must beSome[AnyRef]
        val f_* = grater[F].asObject(dbo)
        f_* must_== f
      }
    }

    "persist and retrieve sorted things" in {

      val arbitraryOrdering = new Ordering[String] {
        def compare(x: String, y: String) = if (x.length == y.length) x.compare(y) else x.length.compare(y.length)
      }

      val expectedOrder = Seq("kings", "ships", "shoes", "cabbages", "sealing wax")
      val walrus = Walrus(Seq("shoes", "ships", "sealing wax", "cabbages", "kings").sorted(arbitraryOrdering))

      walrus.manyThings mustEqual expectedOrder
      "a case class with a sorted list" in {
        val dbo: MongoDBObject = grater[Walrus[String]].asDBObject(walrus)
        dbo.get("manyThings") must beSome[AnyRef]

        val walrus_* = grater[Walrus[String]].asObject(dbo)
        walrus_*.manyThings mustEqual expectedOrder
      }

      "a case class with a sorted list of case classes with sorted lists" in {
        val expectedOrder2 = Seq("is", "and", "hot", "sea", "the", "why", "boiling")
        val walrus2 = Walrus(Seq("and", "why", "the", "sea", "is", "boiling", "hot").sorted(arbitraryOrdering))
        walrus2.manyThings mustEqual expectedOrder2
        val expectedOrder3 = Seq("?", "and", "have", "pigs", "wings", "whether")
        val walrus3 = Walrus(Seq("and", "whether", "pigs", "have", "wings", "?").sorted(arbitraryOrdering))
        walrus3.manyThings mustEqual expectedOrder3

        val expectedHerd = Seq(walrus2, walrus3, walrus)
        val herd = Walrus(manyThings = Seq(walrus, walrus2, walrus3).sorted(new Ordering[Walrus[String]] {
          def compare(x: Walrus[String], y: Walrus[String]) = y.manyThings.length.compare(x.manyThings.length)
        }))
        herd.manyThings mustEqual expectedHerd

        val dbo: MongoDBObject = grater[Walrus[Walrus[String]]].asDBObject(herd)
        dbo.get("manyThings") must beSome[AnyRef]

        val herd_* = grater[Walrus[Walrus[String]]].asObject(dbo)
        herd_*.manyThings mustEqual expectedHerd
        herd_*.manyThings(0).manyThings mustEqual expectedOrder2
        herd_*.manyThings(1).manyThings mustEqual expectedOrder3
        herd_*.manyThings(2).manyThings mustEqual expectedOrder
      }
    }
  }



  "usage example for the README" should {
    val deflate_me = evil_empire

    "print out some sample JSON" in {
      val deflated = grater[Company].asDBObject(deflate_me)
      val inflated = grater[Company].asObject(deflated)
      inflated.copy(departments = Map.empty) must_== deflate_me.copy(departments = Map.empty)
      inflated.departments("MoK") must_== deflate_me.departments("MoK")
      inflated.departments("FOSS_Sabotage") must_== deflate_me.departments("FOSS_Sabotage")
    }
  }
}
