# Vragen/Dingen die ik tegen kom in code

# `Data/Output`: Beschrijving van data

- `participant`: Participant nummer.

- `trial`: Trial nummer.

- `trial_start`: Timestamp die het begin van de trial aangeeft.

- `timestamp`: Timestamp van de huidige regel in het GEHELE experiment.

- `timestamp_relative`: Timestamp relatief aan trial (eerste regel van een trial = 0)

- `event`: Beschrijving event van de regel.

- `x_left	y_left	validity_left`: Informatie linkeroog.

- `x_right	y_right	validity_right`: Informatie rechteroog.

- `can_look`: Placeholder, voor later om aan te geven of de participant op het tijdsmoment van die regel al naar het juiste plaatje kan kijken (TRUE) of niet (FALSE).

- `looking_at_correct`: Placeholder, voor later om aan te geven of de participant op het tijdsmoment van die regel naar het juiste plaatje kijkt (TRUE) of niet (FALSE)

- `pos1	pos2	pos3	pos4`: Placeholders, hier komen de namen van de plaatjes van de trials.


# EyeTracking

## File/Data Info

-   `.tsv` files: Eye-tracker output (Each row == 1 sample).

-   `.csv` files: OpenSesame output (turn-taking experiment).

    -   1000ms: Fixation.

    -   2000ms: 4 pictures.

    -   Audio with 2 cue words.

    -   Participant replies and presses space on keyboard, item ends.

-   More `.csv` files than `.tsv`, enough to test on.

## Questions to answer

1.  Als cue-word 1 **begint**, wanneer kijkt men naar doelwoord (Early/Late conditie).
2.  Als cue-word 2 **begint**, kijkt men dan al naar doelwoord (Early/Late conditie).
3.  In toekomst (nu nog niet data voor): Wordt het juiste antwoord gegeven en wanneer?
