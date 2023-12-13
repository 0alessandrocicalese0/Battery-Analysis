
# Guida Github

## Clonare il repository:

*  Ogni membro del gruppo deve clonare il repository sul proprio computer usando il comando git clone seguito dall'URL del repository.

			git clone <URL_del_repository>
	 
## Lavorare sui file:

* Ogni membro del gruppo può ora modificare i file all'interno della loro copia locale del repository. 
	Una volta apportate le modifiche, aggiungi i file modificati al "staging area" con git add.

 			git add nome_del_file

* Poi, esegui il commit delle modifiche con un messaggio descrittivo.

			git commit -m "Descrizione delle modifiche"

## Push e Pull:

* Per condividere le modifiche con gli altri membri del gruppo, esegui il push delle modifiche sul repository remoto. 

  		git push origin branch_name

* Per aggiornare il proprio file modificato da altri sul repository utilizzare il comando pull.

			git pull origin branch_name

