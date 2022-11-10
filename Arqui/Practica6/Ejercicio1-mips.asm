# Hernández Navarro Armando - 317340347
# Juan Carlos Zenteno Pompa - 316251608
.data
	

.text 
	## Inicio del programa
	main:
	
		
		addi $a1, $zero, 150
	
		jal ejercicio1
		
		## Finalización del programa. Sin esta linea, el programa nunca termina
		li $v0, 10
		syscall 
		
	
	#Declaración de la funcion
	
	#Copia el valor del registro $a1 al registro $v0
	ejercicio1: 
		
		add $v0, $zero, $a1 
	
		jr $ra
		


