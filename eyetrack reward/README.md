# Scripts de R para eyetrack reward
Para proceso de eyetrack en la maquina virtual, antes y después.

## 1. Script para pre-proceso NIH 21 años

> Nombre del script `script_eyetrack_rename.r`

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


# 2. Script para verificar nombres de archivo

> El nombre del script es `script_eyetrack_ids.r`

Es muy similar al anterior pero permite revisar si van a existir o no conflictos con los nombres de los archivos.

Si se quiere renombrar se usa el script anterior, con este se crea una Excel para parear el nombre del 
archivo al id y detección de anomalías

1. Chequea si el nombre empieza por un cero
2. Chequea si hay un espacio en el nombre
3. Chequea si un id tiene más de 1 archivo
4. Chequea si al correr la maquina virtual habrá conflicto de nombres

Esto sería un conflicto de nombre

    123reward.eyd    -> 123reward1.eyd
    123reward_2.eyd  -> 123reward2.eyd

Se crea el Excel en el working directory



# 3. Script para crear base datos resultados

> El script es `script_eyetrack_bind_3.0.R`

Este script usa los archvios que ya están procesados en la maquina virtual

> **Falta escribir la reseña de como funciona**




