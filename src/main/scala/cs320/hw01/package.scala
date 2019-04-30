package cs320

package object hw01 extends Homework01 {
  // Problem 1
  def dollar2won(dollar: Int): Int = dollar * 1100
  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = a * b * c
  def isEven(num: Int): Boolean = (num % 2) == 0
  def isOdd(num: Int): Boolean = (num % 2) == 1
  def gcd(a: Int, b: Int): Int = {
    if(a.abs < b.abs) {
      gcd(b, a)
    }else if (b == 0) {
      a.abs
    }else{
      gcd(b,a%b)
    }
  }
  def lcm(a: Int, b: Int): Int = {
    val gcd_num= gcd(a,b)
    if (gcd_num == 0){
      0
    }
    ((a*b) / gcd_num).abs
  }

  // Problem 2
  def numOfHomework(course: COURSE): Int = course match {
    case CS320(quiz, homework) => homework
    case CS311(h) => h
    case CS330(p,h) => h
  }
  def hasProjects(course: COURSE): Boolean = course match {
    case CS320(q,h) => false
    case CS311(h) => false
    case CS330(p,h) if (p >= 2) => true
  }
  // Problem 3
  def namePets(pets: List[String]): List[String] = {
    pets.map{ x=>
      if(x== "dog") "happy"
      else if(x== "cat") "smart"
      else if(x== "pig") "pinky"
      else x
    }
  }
  def giveName(oldName: String, newName: String): List[String] => List[String] = {
    (before: List[String] )=> before.map{ x=>
        if(x== oldName) newName
        else x
    }
  }

  def tests: Unit = {
    test(dollar2won(1), 1100)
    test(volumeOfCuboid(1, 2, 3), 6)
    test(isEven(10), true)
    test(isOdd(10), false)
    test(gcd(123, 245), 1)
    test(lcm(123, 245), 30135)
    test(numOfHomework(CS320(quiz = 4, homework = 3)), 3)
    test(hasProjects(CS320(quiz = 3, homework = 9)), false)
    test(namePets(List("dog", "cat", "pig")), List("happy", "smart", "pinky"))
    test(giveName("bear", "pooh")(List("pig", "cat", "bear")), List("pig", "cat", "pooh"))

    /* Write your own tests */
    test(dollar2won(0), 0)
    test(volumeOfCuboid(1, 0, 3), 0)
    test(volumeOfCuboid(2, 2, 2), 8)
    test(isEven(0), true)
    test(isOdd(1), output = true)
    test(gcd(123, 246), 123)
    test(gcd(5, 2), 1)
    test(lcm(121, 11), 121)
    test(lcm(1, 11), 11)
    test(numOfHomework(CS311(homework = 1)), 1)
    test(hasProjects(CS330(projects = 3, homework = 9)), output = true)
    test(hasProjects(CS330(projects = 0, homework = 9)), output = false)
    test(namePets(List("dog", "pig", "bird", "pig")), List("happy", "pinky","bird", "pinky"))
    test(giveName("pig", "fat")(List("pig", "cat", "bear")), List("fat", "cat", "bear"))
  }
}