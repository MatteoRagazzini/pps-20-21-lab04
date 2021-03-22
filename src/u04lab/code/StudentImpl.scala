package u04lab.code
import Lists.List
import Lists.List._
import Lists.List.{Cons,Nil}

case class StudentImpl(name: String, year:Int ) extends Student() {
  private var coursesList: List[Course] = Nil()

  override def enrolling(course: Course*): Unit = course.foreach(c => coursesList = append(Cons(c, Nil()), coursesList))

  override def courses: Lists.List[String] = map(coursesList)((c: Course) => c.name)

  override def hasTeacher(teacher: String): Boolean = contains(map(coursesList)((c: Course) => c.teacher))(teacher)
}
