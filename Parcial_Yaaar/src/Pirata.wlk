import Barco.*
import Misiones.*
import Ciudad.*

class Pirata {
	
	const property items
	var property nivelDeEbriedad
	var property monedas
	var property pirataInvitador = self
	
	
	method esUtilParaMision(unaMision) {
		unaMision.esUtil(self)
	}
	
	method tiene(unItem) {
		return items.contains(unItem)
	}
	
	method cantItems() {
		return items.size()
	}
	
	method animarseASaquear(unaVictima) {
		return unaVictima.esSaqueablePor(self)
	}
	
	method pasadoDeGrogXD() {
		return self.suNivelDeEbriedadEstaPorEncimaDe(90)
	}
	
	method suNivelDeEbriedadEstaPorEncimaDe(unNumero){
		return nivelDeEbriedad >= unNumero
	}
	
	method anclar(){
		self.puedeGastarMonedas()
		monedas -= 1
		nivelDeEbriedad += 5
	}
	
	method puedeGastarMonedas(){
		if(monedas > 0){}
		else { throw new Exception(message = "El pirata esta pobre")}
	}
	
	
	
}

class EspiaDeLaCorona inherits Pirata{
	
	override method pasadoDeGrogXD(){
		return false
	}
	
	override method animarseASaquear(unaVictima){
		return super(unaVictima) && self.tiene("permisoDeLaCorona") 
	}
	
}
