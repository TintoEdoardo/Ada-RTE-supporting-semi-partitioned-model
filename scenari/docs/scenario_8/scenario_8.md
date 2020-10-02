# Scenario 8

Torna all'[indice](../index.md)

Questo scenario, costruito a partire dallo [scenario 7](../scenario_7/scenario_7.md), riguarda il definire le azioni da intraprendere alla rilevazione di un *idle tick*. La descrizione di cosa sia un *idle tick* è già stata esposta nello [scenario 6](../scenario_6/scenario_6.md).

Alla rilevazione di un *idle tick*, bisogna:
- [ ] se il core sta eseguendo in modalità `LOW` => nessuna azione di reazione;
- [ ] se il core sta eseguendo in modalità `HIGH`
  - [ ] la modalità di esecuzione deve essere portata a `LOW`
  - [ ] i tasks nella `Discarded_Thread_Table` devono essere ripristinati
    - [ ] Cosa si intende per ripristinati? Si rimanda allo [scenario 6](../scenario_6/scenario_6.md)
    - [ ] sia per i tasks `Low_Crit_First_CPU` che per quelli `High_Crit_First_CPU`, il monitoraggio del consumo del budget deve essere relativo al loro `Low_Critical_Budget`
      - [ ] sono esattamente le condizionalità esposte nello [scenario 7](../scenario_7/scenario_7.md) nel caso che il core stia eseguendo in modalità `LOW`. 

Torna all'[indice](../index.md)