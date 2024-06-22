package proyecto

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.collection.parallel.CollectionConverters._
import scala.concurrent.Future

class ItinerariosPar {
  type Aeropuertos = List[Aeropuerto]
  type Vuelos = List[Vuelo]

  private val itinerariosSeq = new Itinerario()

  def itinerariosPar(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String) => Future[List[List[Vuelo]]] = {
    (cod1: String, cod2: String) => Future {
      itinerariosSeq.itinerarios(vuelos, aeropuertos)(cod1, cod2)
    }
  }

  def itinerariosTiempoPar(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String) => Future[List[List[Vuelo]]] = {
    def calcularDuracionVuelo(vuelo: Vuelo): Int = {
      val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
      val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get

      val salidaEnMinutos = vuelo.HS * 60 + vuelo.MS
      val llegadaEnMinutos = vuelo.HL * 60 + vuelo.ML

      val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT) / 100
      val diferenciaGMTEnMinutos = diferenciaGMT * 60

      val duracionEnMinutos = llegadaEnMinutos - (salidaEnMinutos + diferenciaGMTEnMinutos)
      if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos
    }

    def calcularTiempoEspera(vuelo1: Vuelo, vuelo2: Vuelo): Int = {
      val llegadaEnMinutos = vuelo1.HL * 60 + vuelo1.ML
      val salidaEnMinutos = vuelo2.HS * 60 + vuelo2.MS

      val esperaEnMinutos = salidaEnMinutos - llegadaEnMinutos
      if (esperaEnMinutos < 0) esperaEnMinutos + 1440 else esperaEnMinutos
    }

    def calcularTiempoTotal(itinerario: List[Vuelo]): Int = {
      val tiemposDeVuelo = itinerario.map(calcularDuracionVuelo)
      val tiemposDeEspera = itinerario.zip(itinerario.tail).map { case (v1, v2) => calcularTiempoEspera(v1, v2) }
      tiemposDeVuelo.sum + tiemposDeEspera.sum
    }

    (cod1: String, cod2: String) => Future {
      val itinerarios = itinerariosSeq.itinerarios(vuelos, aeropuertos)(cod1, cod2)
      itinerarios.par.map(it => (it, calcularTiempoTotal(it)))
        .toList
        .sortBy(_._2)
        .take(3)
        .map(_._1)
    }
  }

  def itinerariosEscalasPar(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String) => Future[List[List[Vuelo]]] = {
    def calcularEscalas(itinerario: List[Vuelo]): Int = {
      val escExp = itinerario.count(_.Dst != cod2)
      val escTec = itinerario.map(_.Esc)
      escExp + escTec.sum
    }

    def encontrarMenor(pivote: List[Vuelo], its: List[List[Vuelo]]): Boolean = {
      its.forall(calcularEscalas(pivote) <= calcularEscalas(_))
    }

    def buscarVuelo(busqueda: List[Vuelo], its: List[List[Vuelo]]): List[Vuelo] = {
      its.find(it => calcularEscalas(it) == calcularEscalas(busqueda) && it.length < busqueda.length).getOrElse(busqueda)
    }

    def minimoEscalasAux(its: List[List[Vuelo]], itsFiltrada: List[List[Vuelo]]): List[List[Vuelo]] = {
      its match {
        case Nil => Nil
        case h :: t =>
          if (encontrarMenor(h, itsFiltrada)) {
            val menor = buscarVuelo(h, itsFiltrada)
            menor :: minimoEscalasAux(t, itsFiltrada.filter(_ != menor))
          } else {
            minimoEscalasAux(t, itsFiltrada)
          }
      }
    }

    (cod1: String, cod2: String) => Future {
      val itsAll = itinerariosSeq.itinerarios(vuelos, aeropuertos)(cod1, cod2)
      minimoEscalasAux(itsAll, itsAll)
    }
  }

  def itinerariosAirePar(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String) => Future[List[List[Vuelo]]] = {
    def calcularDuracionVuelo(vuelo: Vuelo): Int = {
      val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
      val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get

      val salidaEnMinutos = vuelo.HS * 60 + vuelo.MS
      val llegadaEnMinutos = vuelo.HL * 60 + vuelo.ML

      val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT) / 100
      val diferenciaGMTEnMinutos = diferenciaGMT * 60

      val duracionEnMinutos = llegadaEnMinutos - (salidaEnMinutos + diferenciaGMTEnMinutos)
      if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos
    }

    def calcularTiempoTotal(itinerario: List[Vuelo]): Int = {
      itinerario.map(calcularDuracionVuelo).sum
    }

    (cod1: String, cod2: String) => Future {
      val itsAll = itinerariosSeq.itinerarios(vuelos, aeropuertos)(cod1, cod2)
      val itsConDuracion = itsAll.map(it => (it, calcularTiempoTotal(it)))
      itsConDuracion.sortBy(_._2).map(_._1).take(3)
    }
  }

  def itinerariosSalidaPar(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String, Int, Int) => Future[List[Vuelo]] = {
    def convertirAMinutos(hora: Int, minutos: Int): Int = {
      hora * 60 + minutos
    }

    (cod1: String, cod2: String, horaCita: Int, minCita: Int) => Future {
      val tiempoCita = convertirAMinutos(horaCita, minCita)
      val itsAll = itinerariosSeq.itinerarios(vuelos, aeropuertos)(cod1, cod2)
      val itsValidos = itsAll.filter(it => convertirAMinutos(it.last.HL, it.last.ML) <= tiempoCita)

      if (itsValidos.isEmpty) List()
      else {
        itsValidos.sortBy(it => (tiempoCita - convertirAMinutos(it.last.HL, it.last.ML), -convertirAMinutos(it.head.HS, it.head.MS))).take(1).head
      }
    }
  }
}
