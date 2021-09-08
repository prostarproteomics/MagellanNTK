# Spécifications techniques

## Module process

### Ouverture d'un module, 

* on a la barre de navigation en haut avec à gauche les boutons 'Previous' (<<) et 'Reset' et à droite le bouton 'Next' (>>) à droite
* La barrede navigation affiche les étapes du processus. Elles sont toutes désactivées sauf la première (Description). Le dataset est en "attente" au niveau de la première étape
µ on peut naviguer sur les autres étapes
* Pour les étapes désactivées, les widgets correspondants le sont aussi
* Un reset à ce moment fonctionne déjà et remet le curseur au niveau de la première étape

### Load d'un dataset

* cette action est réalisée par un clic sur le bouton "Start" de l'étape de Description,
* A chaque fois qu'une étape a été validée:
    * le cercle correspondant dans la timeline se colore en vert et est désactivé.
    * toutes les étapes ultérieures sont activées jusq'à la première obligatoire inclus. Les       suivantes restent désactivées car elles nécessitent la validation de l'étape                 obligatoire
    
### Navigation et validation

* si on valide une étape i alors que des étapes i-x n'ont pas été validées, ces dernières sont considérées comme omises. Le repère sur la timeline devient grisé et les widgets sont désactivés
* Si on revient sur une étape omise, un bandeau bleu s'affiche pour l'expliquer
* lorsqu'une étape a été validée, ses widgets gardent leur valeur même si on navigue sur d'autres étapes
* La validation de la dernière étape entraîne la mise à jour de la variable de sortie du module

### Reset

* Un clic sur le bouton 'Reset' a pour actions de:
  * remettre le curseur sur l'étape 1
  * Supprimer le dataset de sortie
  * Désactiver toutes les étapes sauf la première
  * Les widgets des étapes désactivées reprennent leur valeur par défaut
  
### Remote commands

 * Le remoteReset a les mêmes effets que le Reset local
 
## Module Pipeline



