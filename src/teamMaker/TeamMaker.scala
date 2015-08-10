package teamMaker
import util.Random.nextInt
object TeamMaker {
	def main(args: Array[String]): Unit = {
			println("How many students are in the class?")
			var numStudents = inputCheck(20,40)	
			println("How many assignments?")
			var numAssignments = inputCheck(8,12)
			println("number of students : " + numStudents + "and " + " number of assignments : " + numAssignments)
			val list = generateTeams(numStudents, numAssignments)
			if(check(list) == None) println("The teams are \n"  + list.mkString("\n"))
			else println ("Repeated teams exists : " + check(list))
	}
	def inputCheck(lowerBound: Int, upperBound: Int  ) : Int = {
			var num :Int = null.asInstanceOf[Int]
					var bol = true
					while(bol){
						try{  num = Console.readLine.toInt
								if ((num.toInt < lowerBound || num.toInt > upperBound)) println("Print valid number ")
								else bol = false
						} catch{case e:java.lang.NumberFormatException => println("Print valid number ")}}
	num
	}
	def generateTeams(numStudents: Int, numAssignments: Int): List[List[Tuple2[Int, Int]]] = {
			var listOfLists = List[List[Tuple2[Int,Int]]]()
					while(listOfLists.size != numAssignments ){
						var randomList = (new util.Random).shuffle(1 to numStudents)
								var lists = List[Tuple2[Int,Int]]()
								for( a <- 0 to numStudents/2 -1){
									var x = (randomList(a),randomList(numStudents/2  + a))
											if(x._1 > x._2)  x = x.swap
											lists = lists ::: List(x)
								}
						if((check( listOfLists ::: List(lists)) == None))  listOfLists = listOfLists ::: List(lists)
					}
			listOfLists 
	}
	def check(teams: List[List[Tuple2[Int, Int]]]): Option[List[Tuple2[Int, Int]]] = {
			var intersect = Set[(Int,Int)]()
					for(a <- 0 until teams.size){
						var set = teams(a).toSet
								for(b <- a + 1 until teams.size){
									var other = teams(b).toSet
											intersect = intersect ++ other.intersect(set)}
					}
			if(intersect.isEmpty) None
			else  Option(intersect.toList)
	}
}