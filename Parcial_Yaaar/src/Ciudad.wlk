import Pirata.*
import Barco.*

class CiudadCostera {
	
	var habitantes
	
	method esSaqueablePor(unPirata) {
		return unPirata.suNivelDeEbriedadEstaPorEncimaDe(50)
	}
	
	method aumentaSuPoblacion(unNumero){
		habitantes += unNumero
	}
	
	method vulnerableA(unBarco){
		return unBarco.cantTripulantesTotal() >= habitantes * 0.4 || unBarco.tripulacionPasadaDeGrogXD()
	}
}