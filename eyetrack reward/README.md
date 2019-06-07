# Scripts de R para eyetrack reward
Para proceso de eyetrack en la maquina virtual, antes y después.

## 1. Script para pre-proceso NIH 21 años
Script para arreglar los nombres de los archivos EYD de Eyetrack de Reward del
grupo de anemia. Al hacer el registro pueden quedar malos y se hacen de nuevo y se
producen problemas con los nombres, entonces se tiene que precisar si el archivo es 
bloque 1 o 2 u otro 
Se hace antes de pasar por la maquina virtual se queda ok el file id.

por ejemplo se pasa de 

    123reward.eyd    -> 123reward1.eyd
    123reward_2.eyd  -> 123reward2.eyd
 
También para evitar esto

    1232reward.eyd y 123reward2.eyd

va a arrojar el mismo resultado porque el programa conserva el número y borra todas
las letras. Entonces sobre escribe uno de los resultados y se pierde información.
Va con full de comentarios para que se entienda

> Nombre del script `script_eyetrack_rename.r`
