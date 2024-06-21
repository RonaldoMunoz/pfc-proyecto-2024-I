package proyecto

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def findItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      def search(current: String, destination: String, visited: Set[String]): List[List[Vuelo]] = {
        if (current == destination) List(List())
        else {
          for {
            vuelo <- vuelos if vuelo.Org == current && !visited.contains(vuelo.Dst)
            itinerario <- search(vuelo.Dst, destination, visited + current)
          } yield vuelo :: itinerario
        }
      }
      search(cod1, cod2, Set())
    }
    findItinerarios
  }


  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def minimoTiempo(cod1: String, cod2: String): List[List[Vuelo]] = {
      val itsAll = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      itsAll.sortBy(it => calcularTiempoTotal(it, aeropuertos)).take(3)
    }
    minimoTiempo
  }
  def calcularDuracionVuelo(vuelo: Vuelo, aeropuertos: List[Aeropuerto]): Int = {
    val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
    val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get
    val salidaEnMinutos = (vuelo.HS * 60) + vuelo.MS
    val llegadaEnMinutos = (vuelo.HL * 60) + vuelo.ML
    val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT) / 100
    val diferenciaGMTEnMinutos = (diferenciaGMT * 60).toInt
    val duracionEnMinutos = llegadaEnMinutos - (salidaEnMinutos + diferenciaGMTEnMinutos)
    if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos
  }
  def calcularTiempoEspera(vuelo1: Vuelo, vuelo2: Vuelo): Int = {
    val llegadaEnMinutos = vuelo1.HL * 60 + vuelo1.ML
    val salidaEnMinutos = vuelo2.HS * 60 + vuelo2.MS


    val esperaEnMinutos = salidaEnMinutos - llegadaEnMinutos

    if (esperaEnMinutos < 0) esperaEnMinutos + 1440 else esperaEnMinutos
  }
  def calcularTiempoTotal(itinerario: List[Vuelo], aeropuertos: List[Aeropuerto]): Int = {
    val tiemposDeVuelo = itinerario.map(vuelo => calcularDuracionVuelo(vuelo, aeropuertos))
    val tiemposDeEspera = itinerario.zip(itinerario.tail).map { case (v1, v2) => calcularTiempoEspera(v1, v2) }
    tiemposDeVuelo.sum + tiemposDeEspera.sum
  }
  def itinerariosEscalas(vuelos:List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def minimoEscalas(cod1: String, cod2: String): List[List[Vuelo]] = {
      val itsAll = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      def calcularEscalas(itinerario: List[Vuelo]): Int = {
        val escExp = itinerario.count(v => v.Dst != cod2)
        val escTec = itinerario.map(v => v.Esc)
        escExp + escTec.sum
      }
      val itsAllEscalas = itsAll.map(it => (it, calcularEscalas(it)))
      itsAllEscalas.sortBy(_._2).map(_._1).take(3)
    }
    minimoEscalas
  }

  // Definimos la función itinerariosAire
  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {

    // Definimos la función calcularTiempoTotal que calcula el tiempo total de un itinerario
    def calcularTiempoTotal(itinerario: List[Vuelo]): Int = {
      itinerario match {
        case Nil => 0 // Si el itinerario está vacío, el tiempo total es 0
        case _ :: Nil => 0 // Si el itinerario tiene un solo vuelo, el tiempo total es 0
        case vuelo1 :: vuelo2 :: tail => { // Si el itinerario tiene más de un vuelo
          // Buscamos los aeropuertos de origen y destino del vuelo
          val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo1.Org).get
          val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo1.Dst).get

          // Calculamos la salida y llegada en minutos
          val salidaEnMinutos = (vuelo1.HS * 60) + vuelo1.MS
          val llegadaEnMinutos = (vuelo1.HL * 60) + vuelo1.ML

          // Calculamos la diferencia de GMT entre los aeropuertos
          val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT)/100
          val diferenciaGMTEnMinutos = (diferenciaGMT * 60).toInt

          // Calculamos la duración del vuelo en minutos
          val duracionEnMinutos = llegadaEnMinutos - (salidaEnMinutos + diferenciaGMTEnMinutos)
          val duracion = if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos

          // Sumamos la duración del vuelo al tiempo total del resto del itinerario
          duracion + calcularTiempoTotal(vuelo2 :: tail)
        }
      }
    }

    // Definimos la función minimoAire que encuentra los tres itinerarios con menor tiempo en el aire
    def minimoAire(cod1: String, cod2: String): List[List[Vuelo]] = {
      // Obtenemos todos los itinerarios posibles entre los dos aeropuertos
      val istAll = itinerarios(vuelos, aeropuertos)(cod1, cod2)

      // Calculamos el tiempo total de cada itinerario
      val itsAllTime = istAll.map(it => (it, calcularTiempoTotal(it)))

      // Ordenamos los itinerarios por tiempo total y tomamos los tres primeros
      itsAllTime.sortBy(_._2).map(_._1).take(3)
    }

    // Devolvemos la función minimoAire
    minimoAire
  }

  // Definimos la función itinerariosSalida
  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String, Int, Int) => List[List[Vuelo]] = {

    // Definimos la función buscarItinerarios que busca todos los itinerarios posibles desde un origen hasta un destino
    def buscarItinerarios(origen: String, destino: String, visitados: Set[String], caminoActual: List[Vuelo], horaCita: Int): List[List[Vuelo]] = {
      // Si el origen es igual al destino, verificamos si el último vuelo llega antes de la hora de la cita
      if (origen == destino) {
        if (caminoActual.last.HL * 60 + caminoActual.last.ML <= horaCita) List(caminoActual)
        else List()
      } else {
        // Si el origen no es igual al destino, buscamos los vuelos disponibles desde el origen
        val vuelosDisponibles = vuelos.filter(v =>
          v.Org == origen &&
            !visitados.contains(v.Dst) &&
            v.HS * 60 + v.MS > caminoActual.last.HL * 60 + caminoActual.last.ML &&
            v.HL * 60 + v.ML <= horaCita
        )
        // Para cada vuelo disponible, buscamos los itinerarios desde el destino del vuelo hasta el destino final
        vuelosDisponibles.flatMap { vuelo =>
          val nuevosVisitados = visitados + origen
          buscarItinerarios(vuelo.Dst, destino, nuevosVisitados, caminoActual :+ vuelo, horaCita)
        }
      }
    }

    // La función itinerariosSalida toma un origen, un destino y una hora de cita y devuelve los itinerarios que llegan antes de la hora de la cita
    (origen: String, destino: String, horaCita: Int, minCita: Int) => {
      // Convertimos la hora de la cita a minutos
      val tiempoCita = horaCita * 60 + minCita
      // Buscamos los itinerarios desde el origen hasta el destino que llegan antes de la hora de la cita
      val itinerariosEncontrados = vuelos.filter(_.Org == origen).flatMap { vuelo =>
        buscarItinerarios(vuelo.Dst, destino, Set.empty, List(vuelo), tiempoCita)
      }

      // Si no se encontraron itinerarios, devolvemos una lista vacía
      if (itinerariosEncontrados.isEmpty) List()
      else {
        // Si se encontraron itinerarios, devolvemos los que tienen la salida más tarde
        val salidaMasTarde = itinerariosEncontrados.map(it => it.last.HS * 60 + it.last.MS).max
        itinerariosEncontrados.filter(it => it.last.HS * 60 + it.last.MS == salidaMasTarde)
      }
    }
  }


}