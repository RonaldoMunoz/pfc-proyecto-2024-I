package proyecto

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los itinerarios posibles de cod1 a cod2
    def generarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      def buscar(actual: String, destino: String, visitados: Set[String] = Set()): List[List[Vuelo]] = {
        if (actual == destino) List(List())
        else {
          for {
            v <- vuelos if v.Org == actual && !visitados.contains(v.Dst)
            i <- buscar(v.Dst, destino, visitados + actual)
          } yield v::i
        }
      }

      buscar(cod1, cod2)
    }

    generarItinerarios
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    //Recibe vuelos, una lista de vuelos y aeropuertos, una lista de aeropuertos y retorna una funcion que recibe dos strings y retorna una lista de itinerarios
    //Devuelve una función que recibe c1 y c2, códigos de aeropuertos
    //y devuelve una función que devuelve los tres (si los hay) itinerarios que minimizan el tiempo total de viaje
    def calcularDuracionVuelo(vuelo: Vuelo): Int = {
      val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
      // se busca el aeropuerto de origen cuyo codigo sea igual al codigo de origen del vuelo

      val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get
      //  se está buscando un aeropuerto cuyo código (Cod) sea igual al código de destino (Dst) del vuelo.

      val salidaEnMinutos = (vuelo.HS * 60) + vuelo.MS
      // está calculando la hora de salida de un vuelo en minutos.
      val llegadaEnMinutos = (vuelo.HL * 60) + vuelo.ML


      val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT)/100
      // Por lo tanto, esta línea de código está calculando la diferencia de tiempo entre el aeropuerto de origen y el aeropuerto de destino,
      val diferenciaGMTEnMinutos = (diferenciaGMT * 60).toInt
      // y esta línea de código está convirtiendo esa diferencia de tiempo en minutos.
      val duracionEnMinutos = llegadaEnMinutos - (salidaEnMinutos + diferenciaGMTEnMinutos)
      // Por lo tanto, esta línea de código está calculando la duración de un vuelo en minutos.
      if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos
    }
      // se debe a que la duración se calcula en un formato de 24 horas, y si la duración es negativa,
      // significa que el vuelo ha
      // pasado al día siguiente. Agregar 1440 minutos (que es el número total de minutos en un día)
      // ajusta la duración al valor correcto para el día siguiente.

    def calcularTiempoEspera(vuelo1: Vuelo, vuelo2: Vuelo): Int = {

      val llegadaEnMinutos = vuelo1.HL * 60 + vuelo1.ML
      val salidaEnMinutos = vuelo2.HS * 60 + vuelo2.MS

      val esperaEnMinutos = salidaEnMinutos - llegadaEnMinutos

      if (esperaEnMinutos < 0) esperaEnMinutos + 1440 else esperaEnMinutos
    }

    def calcularTiempoTotal(itinerario: List[Vuelo]): Int = {
      val tiemposDeVuelo = itinerario.map(vuelo => calcularDuracionVuelo(vuelo))
      val tiemposDeEspera = itinerario.zip(itinerario.tail).map { case (v1, v2) => calcularTiempoEspera(v1, v2) }
      tiemposDeVuelo.sum + tiemposDeEspera.sum
    }

    def minimoTiempo(cod1: String, cod2: String): List[List[Vuelo]] = {
      val itsAll = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      itsAll.sortBy(it => calcularTiempoTotal(it)).take(3)
    }

    minimoTiempo
  }

  def itinerariosEscalas(vuelos:List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[List[Vuelo]]
  = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el número de escalas
    def minimoEscalas(cod1: String, cod2: String): List[List[Vuelo]] = {
      def calcularEscalas(itinerario: List[Vuelo]): Int = {
        val escExp = itinerario.count(v => v.Dst != cod2)
        // calcula cada escala (cada vuelo que no termine en cod2 es una escala)
        val escTec = itinerario.map(v => v.Esc)
        // calcula las escalas tenicas de cada vuelo
        escExp + escTec.sum
      }

      def encontrarMenor(pivote: List[Vuelo], its: List[List[Vuelo]]): Boolean = {
        its.forall(it => calcularEscalas(pivote) <= calcularEscalas(it))
      }
      // devuelve true si para cada vuelo del itinerario el pivote tiene
      //                      menos escalas

      def buscarVuelo(busqueda: List[Vuelo], its: List[List[Vuelo]]): List[Vuelo] = {
        //buscan el primer itinerario en its que tiene el mismo número de escalas
        // que el itinerario busqueda y que tiene menos vuelos que busqueda.
        val primero = its.find(it => calcularEscalas(it) == calcularEscalas(busqueda) && it.length < busqueda.length)

        primero match {
          case Some(value) => value
          case None => busqueda
        }
      }

      def minimoEscalasAux(its: List[List[Vuelo]], itsFiltrada: List[List[Vuelo]]): List[List[Vuelo]] = {
        its match {
          case Nil => Nil
          case h::t =>
            if (encontrarMenor(h, itsFiltrada)) {
              val menor = buscarVuelo(h, itsFiltrada)
              menor::minimoEscalasAux(t, itsFiltrada.filter(it => it != menor))
            }
            else minimoEscalasAux(t, itsFiltrada)
        }
      }

      val itsAll = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      minimoEscalasAux(itsAll, itsAll)
    }
    minimoEscalas
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def calcularDuracionVuelo(vuelo: Vuelo): Int = {
      val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
      val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get

      val salidaEnMinutos = (vuelo.HS * 60) + vuelo.MS
      val llegadaEnMinutos = (vuelo.HL * 60) + vuelo.ML

      val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT)/100
      val diferenciaGMTEnMinutos = (diferenciaGMT * 60).toInt

      val duracionEnMinutos = llegadaEnMinutos - (salidaEnMinutos + diferenciaGMTEnMinutos)

      if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos
    }

    def calcularTiempoTotal(itinerario: List[Vuelo]): Int = {
      itinerario match {
        case Nil => 0
        case _ :: Nil => 0
        case vuelo1 :: vuelo2 :: tail => calcularDuracionVuelo(vuelo1) + calcularTiempoTotal(vuelo2 :: tail)
      }
    }

    def minimoAire(cod1: String, cod2: String): List[List[Vuelo]] = {
      val istAll = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      val itsAllTime = istAll.map(it => (it, calcularTiempoTotal(it)))
      itsAllTime.sortBy(_._2).map(_._1).take(3)
    }

    minimoAire
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String, Int, Int) => List[List[Vuelo]] = {
    def convertirAMinutos(hora: Int, minutos: Int): Int = {
      hora * 60 + minutos
    }

    def minimaSalida(cod1: String, cod2: String, horaCita: Int, minCita: Int): List[List[Vuelo]] = {
      val tiempoCita = convertirAMinutos(horaCita, minCita)
      val itsAll = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      val itsValidos = itsAll.filter(it => convertirAMinutos(it.last.HL, it.last.ML) <= tiempoCita)

      if (itsValidos.isEmpty) List()
      else {
        val salidaMasTarde = itsValidos.flatMap(it => it.map(v => convertirAMinutos(v.HS, v.MS))).max
        itsValidos.filter(it => it.exists(v => convertirAMinutos(v.HS, v.MS) == salidaMasTarde))
      }
    }

    minimaSalida
  }


}
