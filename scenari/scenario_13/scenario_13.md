# Scenario 13

Questo scenario:
- è costruito a partire dagli stati $`S_1'`$ e $`S_2'`$ dello [scenario 12](../scenario_12/scenario_12.md);
  - e quindi parte dal fatto che **esattamente** una delle due CPU sta eseguendo in *HI-crit* mode;
- riguarda il cambio di criticità `LOW => HIGH` della CPU che sta eseguendo in *LO-crit* mode;
  - producendo quindi una situazione nella quale ambo le CPU stanno eseguendo in *HI-crit* mode;
- riguarda il cambio di criticità `HIGH => LOW` di:
  - prima una CPU qualsiasi, tornando quindi in uno degli stati tra $`S_1'`$ e $`S_2'`$;
  - poi della CPU rimanente, tornando quindi alla *steady mode* $`S`$ definita nello [scenario 12](../scenario_12/scenario_12.md);

### Premesse sugli sottoscenari
Siano:
- **CPU A** quella CPU che sta eseguendo in ***HI**-crit* mode;
- **CPU B** quella CPU che sta eseguendo in ***LO**-crit* mode;

Se **CPU A** = CPU**1** e **CPU B** = CPU**2**, allora lo stato di partenza è $`S_1'`$.\
Se **CPU A** = CPU**2** e **CPU B** = CPU**1**, allora lo stato di partenza è $`S_2'`$.

## Scenario 13.1

Questo sottoscenario, basato su uno degli stati $`S_1'`$ e $`S_2'`$, riguarda il cambio di criticità `LOW => HIGH` della CPU che sta ancora eseguendo in *LO-crit* mode. Ovvero:
- si ha la **CPU A**;
- sulla **CPU B** viene forzato un `CPU_Budget_Exceeded`;
- tutti i task *LO-crit* sulla **CPU B**, che siano `Is_Migrable` o meno, devono essere scartati e inseriti nella `Discarded_Thread_Table`.
  
Bisogna:
- [ ] avere la **CPU A** in ***HI**-crit* mode e la **CPU B** in ***LO**-crit* mode;
  - [ ] quindi sulla **CPU A** ci sono i tasks $`LO_A \cup HI_A`$, mentre sulla **CPU B** ci sono i tasks $`LO_B \cup HI_B \cup MIG_A \cup MIG_B`$
- [ ] forzare un `CPU_Budget_Exceeded` sulla **CPU B**;
- [ ] alla sua rilevazione:
  - [ ] i tasks $`LO_B \cup MIG_A \cup MIG_B`$ nella coda dei pronti della **CPU B** devono essere inseriti nella `Discarded_Thread_Table`;
  - [ ] nella coda dei pronti della **CPU B** devono rimanere soltanto i tasks $`HI_B`$ 
- [ ] verificare il tutto facendo sì che la **CPU A** sia la CPU**1**. Allora, lo stato di partenza sarà:\
    $`Y(1)_1 := LO_1 \cup HI_1`$\
    $`Y(1)_2 := LO_2 \cup HI_2 \cup MIG_2 \cup MIG_1`$\
    $`S_1' := Y(1)_1 \cup Y(1)_2`$

    Mentre lo stato di arrivo sarà

    $`BY(1)_1 := LO_1 \cup HI_1`$\
    $`BY(1)_2 := HI_2`$\
    $`Discarded := MIG_1 \cup MIG_2 \cup LO_2`$\
    $`S_1'' := BY(1)_1 \cup BY(1)_2 \cup Discarded`$

    Si è quindi osservata la transazione $`S_1' \rightarrow S_1''`$

- [ ] verificare il tutto facendo sì che la **CPU A** sia la CPU**2**. Allora, lo stato di partenza sarà:\
    $`Y(2)_1 := LO_1 \cup HI_1 \cup MIG_2 \cup MIG_1`$\
    $`Y(2)_2 := LO_2 \cup HI_2`$\
    $`S_2' := Y(2)_1 \cup Y(2)_2`$

    Mentre lo stato di arrivo sarà

    $`BY(2)_1 := HI_1`$\
    $`BY(2)_2 := LO_2 \cup HI_2`$\
    $`Discarded := MIG_1 \cup MIG_2 \cup LO_1`$\
    $`S_2'' := BY(2)_1 \cup BY(2)_2 \cup Discarded`$

    Si è quindi osservata la transazione $`S_2' \rightarrow S_2''`$
## Scenario 13.2

Questo sottoscenario, basato su uno degli stati $`S_1''`$ e $`S_2''`$, riguarda il cambio di criticità `HIGH => LOW` di una delle due CPU. In questo contesto, la `Discarded_Thread_Table` deve contenere $`MIG_A \cup MIG_B \cup LO_B`$
- se `HIGH => LOW` avviene sulla **CPU A**
  - tutti i task $`MIG_A \cup MIG_B`$ vengono messi nella coda dei pronti della **CPU A**;
  - tutti i restanti tasks $`LO_B`$ vengono messi nella coda dei pronti della **CPU B**;
- se `HIGH => LOW` avviene sulla **CPU B**
  -  tutti i task $`MIG_A \cup MIG_B \cup LO_B`$ vengono messi nella coda dei pronti della **CPU B**.

Bisogna:

- [ ] avere ambo le CPU eseguenti in *HI-crit* mode;
  - [ ] la **CPU A** è quella che **per prima** è entrata in *HI-crit* mode;
- [ ] alla rilevazione di un *idle tick* 
  - [ ] se è sulla **CPU A**:
    - [ ] i tasks $`MIG_A \cup MIG_B`$ nella `Discarded_Thread_Table` vengono messi nella coda dei pronti della **CPU A**;
    - [ ] i tasks $`LO_B`$ nella `Discarded_Thread_Table` vengono messi nella coda dei pronti della **CPU B**;
  - [ ] se è sulla **CPU B**:
    - [ ] i tasks $`MIG_A \cup MIG_B \cup LO_B`$ nella `Discarded_Thread_Table` vengono messi nella coda dei pronti della **CPU B**


- [ ] verificare il tutto facendo sì che la **CPU A** sia la CPU**1**. Allora, lo stato di partenza sarà:\
    $`BY(1)_1 := LO_1 \cup HI_1`$\
    $`BY(1)_2 := HI_2`$\
    $`Discarded := MIG_1 \cup MIG_2 \cup LO_2`$\
    $`S_1'' := BY(1)_1 \cup BY(1)_2 \cup Discarded`$

    - [ ] se l'*idle tick* viene rilevato sulla **CPU B** (cioè la 2), allora lo stato di arrivo sarà:\
    $`Y(1)_1 := LO_1 \cup HI_1`$\
    $`Y(1)_2 := LO_2 \cup HI_2 \cup MIG_2 \cup MIG_1`$\
    $`S_1' := Y(1)_1 \cup Y(1)_2`$

        Si è quindi osservata la transazione $`S_1'' \rightarrow S_1'`$

    - [ ] se l'*idle tick* viene rilevato sulla **CPU A** (cioè la 1), allora lo stato di arrivo sarà:\
    $`Y(2)_1 := LO_1 \cup HI_1 \cup MIG_2 \cup MIG_1`$\
    $`Y(2)_2 := LO_2 \cup HI_2`$\
    $`S_2' := Y(2)_1 \cup Y(2)_2`$

        Si è quindi osservata la transazione $`S_1'' \rightarrow S_2'`$

- [ ] verificare il tutto facendo sì che la **CPU A** sia la CPU**2**. Allora, lo stato di partenza sarà:\
    $`BY(2)_1 := HI_1`$\
    $`BY(2)_2 := LO_2 \cup HI_2`$\
    $`Discarded := MIG_1 \cup MIG_2 \cup LO_1`$\
    $`S_2'' := BY(2)_1 \cup BY(2)_2 \cup Discarded`$

    - [ ] se l'*idle tick* viene rilevato sulla **CPU B** (cioè la 1), allora lo stato di arrivo sarà:\
    $`Y(2)_1 := LO_1 \cup HI_1 \cup MIG_2 \cup MIG_1`$\
    $`Y(2)_2 := LO_2 \cup HI_2`$\
    $`S_2' := Y(2)_1 \cup Y(2)_2`$

        Si è quindi osservata la transazione $`S_2'' \rightarrow S_2'`$

    - [ ] se l'*idle tick* viene rilevato sulla **CPU A** (cioè la 2), allora lo stato di arrivo sarà:\
    $`Y(1)_1 := LO_1 \cup HI_1`$\
    $`Y(1)_2 := LO_2 \cup HI_2 \cup MIG_2 \cup MIG_1`$\
    $`S_1' := Y(1)_1 \cup Y(1)_2`$

        Si è quindi osservata la transazione $`S_2'' \rightarrow S_1'`$