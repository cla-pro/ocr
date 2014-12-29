package net.ocr.image

import net.ocr.common.{StatsBlock, Block}

/**
 * Created by cla on 03.09.2014.
 */
class Statistics {
  def groupStats(blocks: List[Block]): List[StatsBlock] = {
    val mapped = blocks.map(_.length).sortBy((l) => l).groupBy((l) => l).map((p) => (p._1.asInstanceOf[Int], p._2.size))
    val histogram: List[(Int, Int)] = mapped.toList.sortBy(_._1)
    histogram.foldLeft[List[StatsBlock]](Nil)((acc, p) => acc match {
      case Nil => List(StatsBlock(p._2, p._1, p._1))
      case x :: xs if (x.to == p._1 - 1) => StatsBlock(x.size + p._2, x.from, p._1) :: xs
      case _ => StatsBlock(p._2, p._1, p._1) :: acc
    }).reverse
  }

  //def prettyPrintStats(stats: List[StatsBlock]): Unit = stats.foreach(f => println(String.format("%3d => %3d", f.))
}
