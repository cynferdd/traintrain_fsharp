// int, decimal, string, string list, int option
// type VoitureId = int
// type Point = int * int; let p = 1, 2
// type Point' = { X: int; Y: int }; { X = 1; Y = 2 }
// type ShrodingerBool = Alive | Dead | Both
// type Payment = | Cash | Cheque of int | Card of CardNumber * Secret
// type Reserver = Train -> Participant list -> Reservation option
// fun x -> x.Toto


type NonEmptyList<'a> = 'a * 'a list
module NonEmptyList =
    let toList (nl: NonEmptyList<'a>) =
        let (head,tail) = nl
        head::tail

type NumeroVoiture = NumeroVoiture of int

type Voiture = {
    Numero: NumeroVoiture
    NbPlacesTotales: int
    NbPlacesOccupees: int
}

type Train = Train of NonEmptyList<Voiture>

type Reservation = {
    NumeroVoiture: NumeroVoiture
    NbPlacesReservees: int
}

type ReservationErreur =
    | MoinsDUnePlaceReservee
    | CapaciteDepassee

type Reserver = int -> Train -> Result<Reservation, ReservationErreur>

type TauxRemplissage = decimal
type Seuil = Seuil of TauxRemplissage


module Voiture =
    let neDepassePasLaCapacite seuil nbPlaces voiture =
        let tauxOccupation = 
            (decimal voiture.NbPlacesOccupees + decimal nbPlaces)
            /
            (decimal voiture.NbPlacesTotales)

        tauxOccupation < seuil

module Train =
    let init numero nbPlaces =
        let voiture = { Numero = numero; NbPlacesTotales = nbPlaces; NbPlacesOccupees = 0 }
        Train (voiture, [])

    let etVoitureAvecNbPlaces nbPlaces train =
        let (Train (voiture1, autresVoitures)) = train

        
        let (NumeroVoiture dernierNumero) =
            match autresVoitures with
            | [] -> voiture1.Numero
            | _ -> (List.last autresVoitures).Numero

        let nouveauNumero = NumeroVoiture (dernierNumero + 1)
        let voiture = { Numero = nouveauNumero; NbPlacesTotales = nbPlaces; NbPlacesOccupees = 0 }
        

        Train (voiture1, autresVoitures @ [ voiture ])

let monTrain =
    Train.init (NumeroVoiture 10) 100
    |> Train.etVoitureAvecNbPlaces 100
    |> Train.etVoitureAvecNbPlaces 100

module Reservation =
    let reserver seuil : Reserver =
        fun nbPlaces (Train voitures) ->
            match nbPlaces with
            | x when x <= 0 -> Error MoinsDUnePlaceReservee
            | _ ->

                let voitureLibre = 
                    voitures 
                    |> NonEmptyList.toList
                    |> List.tryFind (Voiture.neDepassePasLaCapacite seuil nbPlaces)
                
                match voitureLibre with
                | None -> Error CapaciteDepassee
                | _ -> 
                    Ok { 
                        NumeroVoiture = NumeroVoiture 0
                        NbPlacesReservees = nbPlaces  
                    }

// Reservation.reserver 10 monTrain
// Reservation.reserver 0 monTrain

let reserver100 = Reservation.reserver 1m
let reserver70 =  Reservation.reserver 0.7m


// reserver100 80 monTrain
// reserver70 80 monTrain

// todo : "reserver" ne prend pas qu'un train mais aussi les réservations passées

// todo pour vsCode => installer ionide pour avoir f#
