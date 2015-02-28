package tim_zh.markov

import scala.io.Source
import scala.util.Random

object MarkovChainer extends App {
	def buildMapFromFile(filename: String) = buildMapFromString(Source.fromFile(filename, "windows-1251").mkString)

	def buildMapFromString(str: String) =
		str.
				split("[.!?]+").view.
				map(
		      _.replaceAll("(\\r|\\n|[^\\wа-яА-Я ])+", " ").
				      toLowerCase.
				      split(" +").
				      iterator.
				      sliding(4).
				      filter(
		            _.size >= 4
				      ).
				      foldLeft(Map[String, List[String]]())(
		            (resultMap, slidingSeq) => {
			            val key = slidingSeq(0) + " " + slidingSeq(1)
			            val value = slidingSeq(2) + " " + slidingSeq(3)
			            if (resultMap.contains(key))
				            resultMap + (key -> (value :: resultMap(key)))
			            else
				            resultMap + (key -> List(value))
		            }
				      )
				).
				foldLeft(Map[String, List[String]]())(
		      (resultMap, sentenceMap) =>
			      sentenceMap.foldLeft(resultMap)(
				      (map, entry) =>
					      if (map.contains(entry._1))
						      map + (entry._1 -> (entry._2 ::: map(entry._1)))
					      else
						      map + entry
			      )
				)

	def buildRandomString(key: String, steps: Int, map: Map[String, List[String]]): String =
		if (steps == 0 || !map.contains(key))
			key
		else
			key + " " + buildRandomString(map(key)(Random.nextInt(map(key).size)), steps - 1, map)

	def test(filename: String, steps: Int) {
		val map = buildMapFromFile(filename)
		val keys = map.keySet.toSeq
		val s = buildRandomString(keys(Random.nextInt(keys.size)), steps, map)
		println(s)
	}

	Random.setSeed(System.currentTimeMillis())
	test("t.txt", 34)
}

