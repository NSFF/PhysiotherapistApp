import java.time._

import com.sun.org.apache.xpath.internal.operations.And


class Person extends Ordered[Person]{
  var firstName: String = "default"
  var lastName: String = "default"

  override def toString = firstName + " " + lastName

  def compare(that: Person): Int ={
    val name = lastName.compareToIgnoreCase(that.lastName)

    if (name != 0)
      name
    else
      firstName.compareToIgnoreCase(that.firstName)
  }
}

trait Weight extends Person{
  var weight: Int = 0
}

class Child extends Person with Weight{
  var amountMilkTeeth: Int = 0
}

class Adult extends Person with Weight{
}

class Elderly extends Person with Weight{
  var previousBloodPressure: Int = 0
  var sugarContent: Int = 0
}

class Doctor extends Person{
  var specialisatie: String = "default"
  var listOfAppointments: List[Appointment[Patient]] = List()

  def addAppointment(appointment: Appointment[Patient]) = {
    var tempList: List[Appointment[Patient]] = listOfAppointments.filter(_.compareDate(appointment)) // a list filtered for only 1 date
    val hour: Int = appointment.time(0)
    val minutes: Int = appointment.time(1)

    def verify(l: List[Appointment[Patient]]): Any = {
      if (l.isEmpty){
        println(s"Email: Your appointment has been confirmed at: $hour h $minutes min on ${appointment.date(0)} / ${appointment.date(1)} / ${appointment.date(2)}")
        println(s"with the following description: '${appointment.description}' Under the name of ${appointment.person.patientCategory.toString}")
        appointment.person.addComplaint(appointment.description)

        listOfAppointments = appointment :: listOfAppointments
      } else if((l.head.time(0) * 60 + l.head.time(1) <= hour * 60 + minutes) &&
        (hour * 60 + minutes <= l.head.endTime(0) * 60 + l.head.endTime(1)) ||
        (l.head.time(0) * 60 + l.head.time(1) <= appointment.endTime(0) * 60 + appointment.endTime(1)) &&
        (appointment.endTime(0) * 60 + appointment.endTime(1) <= l.head.endTime(0) * 60 + l.head.endTime(1))){
          println(s"The following slot at: $hour h $minutes min has already been taken.")
      } else if(appointment.person.equals(l.head.person)){
          println(s"${appointment.person.patientCategory.firstName} ${appointment.person.patientCategory.lastName} you have already made an appointment today.")
      } else {
          verify(l.tail)
        }

    }
    verify(tempList)
    sortAppointmentList("date", "asc")
  }


  /* direction = "asc" or "des" */
  def sortAppointmentList(s: String, direction: String) ={
      if (s == "date") {
        listOfAppointments = listOfAppointments.sortBy(r => (r.date(0), r.date(1), r.date(2), r.time(0), r.time(1)))
      } else if (s == "name") {
        listOfAppointments =listOfAppointments.sortBy(r => (r.person.patientCategory.firstName, r.person.patientCategory.lastName))
      } else if (s == "age") {
        listOfAppointments = listOfAppointments.sortBy(r => (r.person.age))
      } else if (s == "previous visit") {
        listOfAppointments = listOfAppointments.sortBy(r => (r.person.previousVisit(0), r.person.previousVisit(1), r.person.previousVisit(2)))
      }
    /* we can easily implement more sorting mechanisms here */

    if(direction == "asc"){
      listOfAppointments = listOfAppointments.reverse
    }
  }


  def printList = {
    def iter(l: List[Appointment[Patient]]) {
      if (l.nonEmpty) {
        println(s"${l.head.date(0)}/ ${l.head.date(1)}/ ${l.head.date(2)} ${l.head.time(0)} h ${l.head.time(1)} min ${l.head.person.patientCategory.firstName} ${l.head.person.patientCategory.lastName} age: ${l.head.person.age}")
        iter(l.tail)
      }
    }
    iter(listOfAppointments)
  }

  def deleteAppointment(appointment: Appointment[Patient]) = {
    listOfAppointments = listOfAppointments.filter(_.!=(appointment))
  }

  def changeAppointment(appointment: Appointment[Patient],  date: Array[Int], time: Array[Int], length: Array[Int]) = {
    // this could also have been done by  reassigning date, time and length of the appointment
    deleteAppointment(appointment)
    addAppointment(new Appointment(appointment.person, date, time, length, appointment.description))
  }
}

/* here would have been all the logs kept for the a certain doctor */
class Log {

}


/* dateOfBirth = Array(year, month, day) */
class Patient(var dateOfBirth: Array[Int]){
  /* default values */
  var email: String = "no e-mail given"
  var previousVisit: Array[Int] = Array(0,0,0)
  var complaintList: List[String] = List()

  /* the person class which can hold extra information specific to that classCategory */
  val patientCategory = patientClass()
  val ID: Int = Patient.makeID()

  def addComplaint(complaint: String) = {
    complaintList = complaint :: complaintList
  }

  def printComplaints= {
    def iter(l: List[String]) {
      if (l.nonEmpty) {
       println(s"${l.head}")
        iter(l.tail)
      }
    }
    iter(complaintList)
  }

  def equals(that: Patient): Boolean ={
    this.ID == that.ID
  }

  /* calculating age when asked*/
  def age: Int ={
    val localDate: LocalDateTime =  LocalDateTime.now()
    val age: Int = localDate.getYear - dateOfBirth(0)

    if (localDate.getDayOfMonth < dateOfBirth(2))
      if (localDate.getMonthValue < dateOfBirth(1) - 1)
        age - 1
      else age
    else
      age
  }

  /* determines what class the patient belongs to */
  private def patientClass() ={
    if (age < 18)
      new Child
    else if (age < 65)
      new Adult
    else
      new Elderly
  }
}

object Patient{
  private var lastUsedID: Int = 0
  private def makeID(): Int ={
    lastUsedID = lastUsedID + 1
    lastUsedID
  }
}
// (year, month, day)
// (hour, minutes)
class Appointment[T](var person: T, var date: Array[Int], var time: Array[Int], var length: Array[Int], var description: String){

  var endTime: Array[Int]= Array(0,0)
  calculateEndTime()

  /* we assume the length of an appointment never goes through another day. Example: 24 hour long operation won't update the endDate */
  private def calculateEndTime() = {
    endTime(1) = time(1) + length(1) // minutes
    endTime(0) = time(0) + length(0) // hours
    if (endTime(1) >= 60) { // modulo
      endTime(1) -= 60
      endTime(0)+= 1
    }
  }

  def compare(that: Appointment[Patient]): Boolean ={
    this.compareDate(that) && this.compareTime(that) && this.compareLength(that) &&
      this.person.equals(that.person)
  }

  def !=(that: Appointment[Patient]): Boolean ={
    !this.compareDate(that) && !this.compareTime(that) && !this.compareLength(that) &&
      !this.person.equals(that.person)
  }

  def compareDate(that: Appointment[Patient]): Boolean ={
    this.date(0) == that.date(0) && this.date(1) == that.date(1) && this.date(2) == that.date(2)
  }

  def compareTime(that: Appointment[Patient]): Boolean ={
    this.time(0) == that.time(0) && this.time(1) == that.time(1)
  }

  def compareLength(that: Appointment[Patient]): Boolean ={
    this.length(0) == that.length(0) && this.length(1) == that.length(1)
  }
}


class DoctorsCompany{
  var listOfDocters: List[Doctor] = List()

  def addDoctor(doctor: Doctor) ={
    listOfDocters = doctor :: listOfDocters
  }

 def searchDoctor(doctor: Doctor): Doctor ={
   def iter(lst: List[Doctor]): Doctor = {
     if (lst.isEmpty)
        throw new IllegalArgumentException
     if (lst.head == doctor)
        lst.head
     else
        iter(lst.tail)
   }
   iter(listOfDocters)
 }

  def makeAppointment(doctor: Doctor, patient: Patient, date: Array[Int], time: Array[Int], length: Array[Int], description: String): Unit ={
    doctor.addAppointment(new Appointment(patient, date, time, length, description))
  }

  def deleteAppointment(doctor: Doctor, patient: Patient, date: Array[Int], time: Array[Int], length: Array[Int], description: String): Unit = {
    doctor.deleteAppointment(new Appointment(patient, date, time, length, description))
  }

  def changeAppointment(doctor: Doctor, patient: Patient, date: Array[Int], time: Array[Int], length: Array[Int], description: String, dateNew: Array[Int], timeNew: Array[Int], lengthNew: Array[Int] ): Unit = {
    doctor.changeAppointment(new Appointment(patient, date, time, length, description), dateNew, timeNew, lengthNew)
  }
}


/* examples */

val p1 = new Patient(Array(2000, 6, 23))
p1.ID
p1.email = "rvcraene@vub.ac.be"
p1.patientCategory.firstName = "Robin"
p1.patientCategory.lastName = "Van Craenenbroek"
p1.toString

p1.age
p1.patientCategory
p1.patientCategory.weight = 50

val p2 = new Patient(Array(1996, 6, 23))
p2.ID
p2.email = "rvcraene@vub.ac.be"
p2.patientCategory.firstName = "Mathias"
p2.patientCategory.lastName = "Pauwels"

p2.age
p2.patientCategory
p2.patientCategory.weight = 70

val p3 = new Patient(Array(1940, 1, 1))
p3.ID
p3.email = "rvcraene@vub.ac.be"
p3.patientCategory.firstName = "Daan"
p3.patientCategory.lastName = "De Korte"

p3.age
p2.patientCategory
/* for some reason do I have a compile error when asking for variables in the subclasses of person within patient.
 The reason for this is probably because my patientClass() has an is else body so the compiler doesn't know which
 type of Subclass of Person it would be when asked for variables.

 I already tried case classes and pattern matching
 */
//p3.patientCategory.previousBloodPressure
//p3.patientCategory.sugarContent
p3.patientCategory.weight = 90

val doc1 = new Doctor
doc1.firstName = "Robin"
doc1.lastName = "Van Craenenbroek"
doc1.toString
doc1.specialisatie = "Subtropische ziekten"

val doc2 = new Doctor
doc2.firstName = "Kevin"
doc2.lastName = "Geeroms"
doc2.specialisatie = "Spier ziekten"

val doc3 = new Doctor
doc3.firstName = "Robin"
doc3.lastName = "Een achternaam"

val company1 = new DoctorsCompany

company1.addDoctor(doc1)
company1.addDoctor(doc2)
company1.listOfDocters
company1.searchDoctor(doc1).firstName
/* de volgende opzoeking geeft express een error om te tonen dat er een illigal argument exception weergegeven wordt */
// company1.searchDoctor(doc3)

company1.makeAppointment(doc1,p1,Array(2018,1,20),Array(13,45),Array(1,15),"I have pain in my stomach")
company1.makeAppointment(doc1,p1,Array(2018,1,20),Array(20,0),Array(0,10),"Strange feeling in my head")
company1.makeAppointment(doc1,p3,Array(2018,1,20),Array(9,45),Array(0,50),"Pain in my foot")
company1.makeAppointment(doc1,p2,Array(2018,1,20),Array(13,45),Array(2,15),"Yellow Eyes")
company1.makeAppointment(doc1,p2,Array(2018,1,21),Array(13,45),Array(2,15),"Blue Blood")
company1.makeAppointment(doc1,p3,Array(2018,1,22),Array(16,45),Array(0,30),"Headache")

/* the complaints get added when an appointment is completed */
p3.printComplaints

doc1.printList
doc1.sortAppointmentList("name", "des")
doc1.printList
doc1.sortAppointmentList("age", "des")
doc1.printList
company1.deleteAppointment(doc1,p1,Array(2018,1,20),Array(13,45),Array(1,15),"I have pain in my stomach")
/* Delete werkt spijtig genoeg niet wegens een foutje bij het vergelijken van de patienten. Omdat change
appointment ook afhankelijk is van deze functie werkt deze ook niet. Momenteel verwijderd hij willekeurig
bepaalde patienten */
doc1.printList //bij deze een bewijs van de bug

/* opmerking: de implementatie is niet compleet bv: geen logs of signalisatie naar de docter toe. Dit
 moet echter niet al te moeilijk zijn. Ik kan een extra functie oproepen van de log classe bij het toevoegen
 van patienten. Vervolgens print ik hetzelfde signaal naar de doctor

 Voorwaarden zoals dat een 65+ niet maandag of woensdag mag komen heb ik ook niet geimplementeerd omdat ik
 geen parametrische oplossing vond zodat de docters niet zelf al te veel code moesten schrijven of dergelijke.
 (Mijn eerste oplossing was met Cases werken maar dit verliep slecht af)
 manueel is het echter niet moeilijk. Je schrijft een extra if else voorwaarde en roept de l.head.person.patientCategory.age
 en vervolgens de datum.

 verbetering: Ik had beter een Date, Time klasse gemaakt. Dit zou mijn code veel mooier hebben kunnen maken. Ik had
  dan ook geprobeerd maar het ging snel fout door comparisons die mis gingen. */
