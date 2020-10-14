import Pirata.*
import Misiones.*
import Ciudad.*

class BarcoPirata {
	
	var misionActual
	const capacidad
	const tripulantes = #{}
	
	method esSaqueablePor(unPirata) {
		return unPirata.pasadoDeGrogXD()
	}
	
	method hayQuorum(){
		return tripulantes.size() >= capacidad * 0.9
	}
	
	method puedeIncorporarse(unPirata){
		return capacidad > self.cantTripulantesTotal() && unPirata.esUtilParaMision(misionActual)
	}
	
	method incorporar(unPirata){
		if(self.puedeIncorporarse(unPirata)){
			tripulantes.add(unPirata)
		} else {
			throw new Exception(message = "No se pudo incorporar al pirata")
		}
	}
	
	method cambiarDeMision(otraMision){
		misionActual = otraMision
		tripulantes.removeAllSuchThat({ unPirata => not(unPirata.esUtilParaMision(otraMision)) })
	}
	
	method elMasEbrio(){
		return tripulantes.max({ unPirata => unPirata.nivelDeEbriedad() })
	}
	
	method anclarEn(unaCiudad){
		tripulantes.forEach({ unPirata => unPirata.anclar() })
		tripulantes.remove(self.elMasEbrio())
		unaCiudad.aumentaSuPoblacion(1)
	}
	
	method esTemible(){
		misionActual.puedeSerCumplidaPor(self)
	}
	
	method alguienTiene(unObjeto){
		return tripulantes.any({ unPirata => unPirata.tiene(unObjeto) })
	}
	
	method vulnerableA(unBarco){
		 return unBarco.cantTripulantesTotal() >= (self.cantTripulantesTotal() * 2)
	}
	
	method cantTripulantesTotal() {
		return self.cantTripulantesDe(tripulantes) 
	}
	
	method cantTripulantesDe(unaTripulacion){
		return unaTripulacion.size()
	}
	
	method tripulacionPasadaDeGrogXD(){
		tripulantes.all({ unPirata => unPirata.pasadoDeGrogXD()})
	}
	
	method tripulantesPasadosDeGrogXD(){
		return tripulantes.filter({ unPirata => unPirata.pasadoDeGrogXD() })
	}
	
	method cantTripulantesPasadosDeGrogXD(){
		return self.cantTripulantesDe(self.tripulantesPasadosDeGrogXD())
	}
	
	method itemsDeUnaTripulacion(unaTripulacion){
		return unaTripulacion.flatMap({ unPirata => unPirata.items() })
	}
	
	method itemsTripulantesPasadosDeGrogXD(){
		return self.itemsDeUnaTripulacion(self.tripulantesPasadosDeGrogXD())
	}
	
	method cantTiposDeItemsTripulantesPasadosDeGrogXD(){
		return self.itemsTripulantesPasadosDeGrogXD().asSet().size()
	}
	
	method tripulanteMasRicoDeUnaTripulacion(unaTripulacion){
		return unaTripulacion.max({ unPirata => unPirata.monedas() })
	}
	
	method tripulanteMasRicoDeLosTripulantesPasadosDeGrogXD(){
		return self.tripulanteMasRicoDeUnaTripulacion(self.tripulantesPasadosDeGrogXD())
	}
	
	method pirataQueMasInvito(){
		return tripulantes.max({ unPirata => self.cantPiratasInvitados(unPirata) })
	}
	
	method cantPiratasInvitados(unPirata){
		return tripulantes.count({ otroPirata => otroPirata.pirataInvitador() == unPirata })
	}
}
