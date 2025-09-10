// Homework 1

// The number of problems solved by myself: 6 out of 8
object HW1 {
  // Problem 1:

  // Solved by myself: Y
  // Time taken: about 5 mins

  // [contract] dollarToWon: Int -> Int
  // [purpose] To convert an integer number (dollar) and returns the integer (won).
  // [tests] dollarToWon(1) => 1393
  //         dollarToWon(10) => 13930
  //         dollarToWon(0) => 0
  def dollarToWon(dollar: Int): Int = {
    val exchangeRate = 1393 // 1 dollar = 1393 won
    dollar * exchangeRate
  }

  // Problem 2:

  // Solved by myself: N (help from JC)
  // Time taken: about 7 mins

  // [contract] maxOfThreeIntegers: (Int, Int, Int) -> Int
  // [purpose] To find the maximum value among three given integers by calling a custom 'max' function.
  // [tests] maxOfThreeIntegers(3, 5, 2) => 5
  //         maxOfThreeIntegers(10, 8, 9) => 10
  //         maxOfThreeIntegers(-1, -10, -5) => -1

  def max(a: Int, b: Int): Int = {
    if (a > b) a else b
  }

  // This function must call the 'max' helper function in a nested way as required.
  def maxOfThreeIntegers(a: Int, b: Int, c: Int): Int = {
    max(a, max(b, c))
  }

  // Problem 3:
  
  // Solved by myself: Y
  // Time taken: about 4 mins

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

  // Helper function to calculate factorial using recursion.
  def factorial(n: Int): Int = {
    if (n <= 0) 1 // Base case: 0! is 1
    else n * factorial(n - 1) // Recursive step
  }

  def combination(n: Int, k: Int): Int = {
    // n! / (k! * (n - k)!)
    if (k < 0 || k > n) 0
    else factorial(n) / (factorial(k) * factorial(n - k))
  }

  // Problem 6:
  // Solved by myself: N (help from website, Gemini)
  // Time taken: about 20 mins

  // a. Define Vehicle type using a sealed trait and case classes.
  sealed trait Vehicle
  case class Bicycle(wheels: Int) extends Vehicle
  case class Car(wheels: Int, windows: Int) extends Vehicle
  case class Airplane(wheels: Int, windows: Int, engines: Int) extends Vehicle

  // b. Define the 'vehicleTax' function.
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

  // c. Define the 'isVehicleSafe' function.
  // [contract] isVehicleSafe: Vehicle -> String
  // [purpose] To check if a vehicle meets the given safety standards.
  // [tests] isVehicleSafe(Bicycle(2)) => "safe"
  //         isVehicleSafe(Car(3, 4)) => "unsafe"
  //         isVehicleSafe(Airplane(18, 20, 4)) => "safe"
  def isVehicleSafe(vehicle: Vehicle): String = {
    val safe = vehicle match {
      case Bicycle(wheels) => wheels < 4
      case Car(wheels, windows) => wheels > 3 && windows > 2
      case Airplane(wheels, windows, engines) => wheels > 2 && windows > 10 && engines > 1
    }
    if (safe) "safe" else "unsafe"
  }


  // ... (8번 문제까지 계속) ...


  // Test cases for all problems
  @main def run(): Unit = {
    println("Running HW1 tests...")

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
    // Test instances for safety checks
    val unsafeCar = Car(4, 2)      // Unsafe: not enough windows
    val unsafeBike = Bicycle(4)     // Unsafe: too many wheels
    val unsafePlane = Airplane(18, 150, 1) // Unsafe: not enough engines

    // b. vehicleTax tests
    assert(vehicleTax(car, 10, 5, 100) == (4 * 10 + 6 * 5), "Test failed for vehicleTax(Car)")
    assert(vehicleTax(bike, 10, 5, 100) == (2 * 10), "Test failed for vehicleTax(Bicycle)")
    assert(vehicleTax(plane, 10, 5, 100) == (18 * 10 + 150 * 5 + 4 * 100), "Test failed for vehicleTax(Airplane)")
    println("Problem 6b tests passed successfully!")

    // c. isVehicleSafe tests
    assert(isVehicleSafe(car) == "safe", "Test failed for isVehicleSafe(safe Car)")
    assert(isVehicleSafe(bike) == "safe", "Test failed for isVehicleSafe(safe Bicycle)")
    assert(isVehicleSafe(plane) == "safe", "Test failed for isVehicleSafe(safe Airplane)")
    assert(isVehicleSafe(unsafeCar) == "unsafe", "Test failed for isVehicleSafe(unsafe Car)")
    assert(isVehicleSafe(unsafeBike) == "unsafe", "Test failed for isVehicleSafe(unsafe Bicycle)")
    assert(isVehicleSafe(unsafePlane) == "unsafe", "Test failed for isVehicleSafe(unsafe Plane)")
    println("Problem 6c tests passed successfully!")


    // ... (8번 문제까지 계속) ...

    // 만약 모든 테스트를 통과하면 마지막에 이 메시지가 보입니다.
    // println("All tests passed!")
  }
}

