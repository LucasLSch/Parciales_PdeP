import Barco.*
import Pirata.*
import Ciudad.*

class Mision {
	
	method puedeSerCumplidaPor(unBarco){
		return unBarco.hayQuorum()
	}
	
	method esUtil()
	
}

class BusquedaDelTesosro inherits Mision {
	
	method esUtil(unPirata) {
		return self.tieneLoNecesario(unPirata) && unPirata.monedas() <= 5 
	}
	
	method tieneLoNecesario(unPirata) {
		return unPirata.tiene("brujula") || unPirata.tiene("mapa") || unPirata.tiene("grogXD")
	}
	
	override method puedeSerCumplidaPor(unBarco){
		return super(unBarco) && unBarco.alguienTiene("llaveDeCofre")
	}
	
}

class ConvertirseEnLeyenda inherits Mision{
	
	const itemLegendario
	
	method esUtil(unPirata) {
		return unPirata.tiene(itemLegendario) && unPirata.cantItems() >= 10
	}
	
}

class Saqueo inherits Mision{
	
	const victima
	var cantMonedasMaximas = 500
	
	method esUtil(unPirata) {
		return unPirata.monedas() < cantMonedasMaximas && unPirata.animarseASaquear(victima)
	}
	
	override method puedeSerCumplidaPor(unBarco){
		return super(unBarco) && victima.vulnerableA(unBarco)
	}
	
}