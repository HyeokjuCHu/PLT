// Homework 1

// The number of problems solved by myself: 6 out of 8
object HW1 {
  // Problem 1:

  // Solved by myself: Y
  // Time taken: about 3 mins

  // [contract] dollarToWon: Int -> Int
  // [purpose] To convert an integer number (dollar) and returns the integer (won).
  // [tests] dollarToWon(1) => 1393
  //         dollarToWon(10) => 13930
  //         dollarToWon(0) => 0
  def dollarToWon(dollar: Int): Int = {
    dollar * 1393
  }

  // Problem 2:

  // Solved by myself: Y (but i asked JC if I am doing right/correctly/legally)
  // Time taken: about 7 mins

  // [contract] maxOfThreeIntegers: (Int, Int, Int) -> Int
  // [purpose] To find the maximum value among three given integers by calling a custom 'max' function.
  // [tests] maxOfThreeIntegers(3, 5, 2) => 5
  //         maxOfThreeIntegers(10, 8, 9) => 10
  //         maxOfThreeIntegers(-1, -10, -5) => -1

  def max(a: Int, b: Int): Int = {
    if (a > b) a else b
  }

  def maxOfThreeIntegers(a: Int, b: Int, c: Int): Int = {
    max(a, max(b, c))
  }

  // Problem 3:
  
  // Solved by myself: Y
  // Time taken: about 3 mins

  // [contract] volumeCuboid: (Int, Int, Int) -> Int
  // [purpose] To find the volume of a cuboid given its length, width, and height.
  // [tests] volumeCuboid(2, 3, 4) => 24
  //         volumeCuboid(5, 5, 5) => 125
  //         volumeCuboid(1, 1, 1) => 1
  def volumeCuboid(length: Int, width: Int, height: Int): Int = {
    length * width * height
  }

   // Problem 4:
  
  // Solved by myself: Y
  // Time taken: about 15 mins

  // [contract] gcd: (Int, Int) -> Int
  // [purpose] To find the greatest common divisor (GCD) of two integers using recursion.
  // [tests] gcd(48, 18) => 6
  //         gcd(101, 103) => 1
  //         gcd(270, 192) => 6
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  // Problem 5:

  // Solved by myself: Y
  // Time taken: about 15 mins

  // [contract] combination: (Int, Int) -> Int
  // [purpose] To calculate the number of combinations (nCk) using a recursive factorial helper function.
  // [tests] combination(4, 2) => 6
  //         combination(5, 3) => 10
  //         combination(10, 0) => 1
  def factorial(n: Int): Int = {
    if (n <= 0) 1
    else n * factorial(n - 1)
  }

  def combination(n: Int, k: Int): Int = {
    if (k < 0 || k > n) 0
    else factorial(n) / (factorial(k) * factorial(n - k))
  }

  // Problem 6:
  // Solved by myself: N(with help from Gemini)
  // Time taken: about 20 mins

  trait Vehicle
  case class Bicycle(wheels: Int) extends Vehicle
  case class Car(wheels: Int, windows: Int) extends Vehicle
  case class Airplane(wheels: Int, windows: Int, engines: Int) extends Vehicle

  // [contract] vehicleTax: (Vehicle, Int, Int, Int) -> Int
  // [purpose] To calculate the total tax for a vehicle based on the number of its components.
  // [tests] vehicleTax(Car(4, 6), 10, 5, 100) => 70
  //         vehicleTax(Airplane(18, 20, 4), 10, 5, 100) => 680
  def vehicleTax(vehicle: Vehicle, wheelTax: Int, windowTax: Int, engineTax: Int): Int = {
    vehicle match {
      case Bicycle(wheels) => wheels * wheelTax
      case Car(wheels, windows) => (wheels * wheelTax) + (windows * windowTax)
      case Airplane(wheels, windows, engines) => (wheels * wheelTax) + (windows * windowTax) + (engines * engineTax)
    }
  }

  // [contract] isVehicleSafe: Vehicle -> String
  // [purpose] To check if a vehicle meets the given safety standards.
  // [tests] isVehicleSafe(Bicycle(2)) => "safe"
  //         isVehicleSafe(Car(3, 4)) => "unsafe"
  //         isVehicleSafe(Airplane(18, 20, 4)) => "safe"
  def isVehicleSafe(vehicle: Vehicle): String = {
    if (vehicle match {
      case Bicycle(wheels) => wheels < 4
      case Car(wheels, windows) => wheels > 3 && windows > 2
      case Airplane(wheels, windows, engines) => wheels > 2 && windows > 10 && engines > 1
    }) "safe" else "unsafe"
  }

  // Problem 7:
  // Solved by myself: Y
  // Time taken: about 10 mins

  // [contract] nameAlphabet: List[Char] -> List[String]
  // [purpose] To convert a list of characters to a corresponding list of names based on specific rules.
  // [tests] nameAlphabet(List('a', 'b', 'c')) => List("alice", "unnamed", "cherry")
  //         nameAlphabet(List('j', 'k')) => List("jc", "kate")
  def nameAlphabet(chars: List[Char]): List[String] = {
    chars.map {
      case 'a' => "alice"
      case 'c' => "cherry"
      case 'j' => "jc"
      case 'k' => "kate"
      case _   => "unnamed"
    }
  }

  // Problem 8:
  // Solved by myself: N(with help from Gemini)
  // Time taken: about 15 mins

  // [contract] updateName: (String, String, List[String]) -> List[String]
  // [purpose] To replace all occurrences of a specific string with a new one in a list, using recursion.
  // [tests] updateName("cherry", "claire", List("jc", "cherry", "kate")) => List("jc", "claire", "kate")
  //         updateName("a", "b", List("a", "c", "a")) => List("b", "c", "b")
  def updateName(oldName: String, newName: String, names: List[String]): List[String] = {
    names match {
      case Nil => Nil
      case head :: tail =>
        (if (head == oldName) newName else head) :: updateName(oldName, newName, tail)
    }
  }


  // Test cases for all problems
  @main def run(): Unit = {
    // Problem 1 test cases
    assert(dollarToWon(1) == 1393, "Test failed for dollarToWon(1)")
    assert(dollarToWon(10) == 13930, "Test failed for dollarToWon(10)")
    assert(dollarToWon(0) == 0, "Test failed for dollarToWon(0)")
    println("Problem 1 tests passed successfully!")

    // Problem 2 test cases
    assert(maxOfThreeIntegers(3, 5, 2) == 5, "Test failed for maxOfThreeIntegers(3, 5, 2)")
    assert(maxOfThreeIntegers(10, 8, 9) == 10, "Test failed for maxOfThreeIntegers(10, 8, 9)")
    assert(maxOfThreeIntegers(-1, -10, -5) == -1, "Test failed for maxOfThreeIntegers(-1, -10, -5)")
    assert(maxOfThreeIntegers(7, 7, 1) == 7, "Test failed for maxOfThreeIntegers with equal values")
    println("Problem 2 tests passed successfully!")

    // Problem 3 test cases
    assert(volumeCuboid(2, 3, 4) == 24, "Test failed for volumeCuboid(2, 3, 4)")
    assert(volumeCuboid(5, 5, 5) == 125, "Test failed for volumeCuboid(5, 5, 5)")
    assert(volumeCuboid(1, 1, 1) == 1, "Test failed for volumeCuboid(1, 1, 1)")
    println("Problem 3 tests passed successfully!")

    // Problem 4 test cases
    assert(gcd(48, 18) == 6, "Test failed for gcd(48, 18)")
    assert(gcd(101, 103) == 1, "Test failed for gcd(101, 103)")
    assert(gcd(270, 192) == 6, "Test failed for gcd(270, 192)")
    println("Problem 4 tests passed successfully!")

    // Problem 5 test cases
    assert(combination(4, 2) == 6, "Test failed for combination(4, 2)")
    assert(combination(5, 3) == 10, "Test failed for combination(5, 3)")
    assert(combination(10, 0) == 1, "Test failed for combination(10, 0)")
    println("Problem 5 tests passed successfully!")

    // Problem 6 test cases
    val car = Car(4, 6)
    val bike = Bicycle(2)
    val plane = Airplane(18, 150, 4)
    val unsafeCar = Car(4, 2)
    val unsafeBike = Bicycle(4)
    val unsafePlane = Airplane(18, 150, 1)

    assert(vehicleTax(car, 10, 5, 100) == (4 * 10 + 6 * 5), "Test failed for vehicleTax(Car)")
    assert(vehicleTax(bike, 10, 5, 100) == (2 * 10), "Test failed for vehicleTax(Bicycle)")
    assert(vehicleTax(plane, 10, 5, 100) == (18 * 10 + 150 * 5 + 4 * 100), "Test failed for vehicleTax(Airplane)")

    assert(isVehicleSafe(car) == "safe", "Test failed for isVehicleSafe(safe Car)")
    assert(isVehicleSafe(bike) == "safe", "Test failed for isVehicleSafe(safe Bicycle)")
    assert(isVehicleSafe(plane) == "safe", "Test failed for isVehicleSafe(safe Airplane)")
    assert(isVehicleSafe(unsafeCar) == "unsafe", "Test failed for isVehicleSafe(unsafe Car)")
    assert(isVehicleSafe(unsafeBike) == "unsafe", "Test failed for isVehicleSafe(unsafe Bicycle)")
    assert(isVehicleSafe(unsafePlane) == "unsafe", "Test failed for isVehicleSafe(unsafe Plane)")
    println("Problem 6 tests passed successfully!")

    // Problem 7 test cases
    val testList1 = List('a', 'b', 'c', 'j', 'x', 'k')
    val expectedList1 = List("alice", "unnamed", "cherry", "jc", "unnamed", "kate")
    assert(nameAlphabet(testList1) == expectedList1, "Test failed for nameAlphabet 1")
    val testList2 = List('z', 'y', 'x')
    val expectedList2 = List("unnamed", "unnamed", "unnamed")
    assert(nameAlphabet(testList2) == expectedList2, "Test failed for nameAlphabet 2")
    val testList3 = List()
    val expectedList3 = List()
    assert(nameAlphabet(testList3) == expectedList3, "Test failed for empty list")
    println("Problem 7 tests passed successfully!")

    // Problem 8 test cases
    val names1 = List("jc", "cherry", "kate", "cherry")
    val updatedNames1 = List("jc", "claire", "kate", "claire")
    assert(updateName("cherry", "claire", names1) == updatedNames1, "Test failed for updateName 1")
    val names2 = List("a", "b", "c")
    assert(updateName("d", "e", names2) == names2, "Test failed for updateName with no match")
    val names3 = List()
    assert(updateName("a", "b", names3).isEmpty, "Test failed for updateName with empty list")
    println("Problem 8 tests passed successfully!")

    println("All tests passed successfully!")
  }
}

