package effects.instance

import effects.{All, Functor}
import org.scalatest.funsuite.AnyFunSuite

class LensTest extends AnyFunSuite {

  case class Address(street: String, no: Int, Postcode: Int)
  case class Person(name: String, address: Address)
  case class Position(role: String, person: Person)

  val position: Position = Position("it", Person("Mark", Address("here", 123, 4434)))

  test("lens as test") {
    val personAddressL: Lens[Person, Address] =  new Lens[Person, Address] {
        override def apply[F[_]](f: Address => Functor[F, Address]): Person => F[Person] = {
          (person: Person) => {
            val x: Functor[F, Address] = f(person.address)
            val res: F[Person] = x.map(a => person.copy(address = a))
            res
          }

        }
      }


    val getPersonAddress = (person: Person) => Lens.view(personAddressL, person)

    println(getPersonAddress(position.person))

    val modifyPersonAddress = (f: Address => Address, p:Person) => Lens.over(personAddressL, f, p)

    val setPersonAddress = (a: Address, p: Person) => Lens.over(personAddressL, All.const(a), p)

    print(setPersonAddress(position.person.address.copy(street = "Copy"), position.person) )
  }


  test("lens creation") {

    val personAddress: Lens[Person, Address] = Lens.lens((_:Person).address)((p:Person) => (a:Address) => p.copy(address = a))
    val addressStreet: Lens[Address, String] = Lens.lens((_:Address).street)((a:Address) => (s:String) => a.copy(street = s))
    println(Lens.view(personAddress, position.person))
    assert(Lens.view(personAddress, position.person) == Address("here", 123, 4434))
    val x = (a:Int) => a + 1
//    personAddress andThen addressStreet

//    val personStreet = personAddress.
  }

//  test("lens getter impl test") {
//    val personAddressL:LensGetter[Person, Address] = {
//      (f: Address => Const[Address, Address]) => {
//        (person:Person) => {
//          val x = f(person.address)
//          val y = x.getConst
//          Const(y)
//        }
//      }
//    }
//
//    val a = Lens.view3(personAddressL, position.person)
//    println(a)
//  }
//
//  test("lens setter impl test") {
//    val personNameL:LensModify[Person, String] = {
//      (f: String => Identity[String]) => {
//        (person: Person) => {
//          val x = f(person.name)
//          val a = x.runIdentity
//          val p = person.copy(name= x.runIdentity)
//          Identity(p)
//        }
//      }
//    }
//
//    val a = Lens.over2(personNameL, All.const[String, String]("Hello"), position.person)
//    println(a)
//  }
}
