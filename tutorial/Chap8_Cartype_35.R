dat1=matrix(
c('Acura-Legend',	24760,	'Japan',	'Medium',	20,  3,
'Buick-Century',	13150,	'USA',	'Medium',	21, 2,
'Chevrolet-Beretta',	10320,	'USA',	'Compact',	26, 4,
'Chrysler-Le-Baron',	10945,	'USA',	'Compact',	25, 4,
'Chrysler-Le-Baron-Coupe',	12495,	'USA',	'Medium',	22, 2,
'Dodge-Daytona',	9745,	'USA',	'Sporty',	27, 3,
'Dodge-Grand-Caravan',	15395,	'USA',	'Van',	18,  2,
'Eagle-Premier',	15350,	'USA',	'Medium',	22, 4,
'Eagle-Summit',	8895,	'USA',	'Small',	33, 5,
'Ford-Aerostar',	12267,	'USA',	'Van',	18, 1,
'Ford-Escort',	7402,	'USA',	'Small',	33, 4,
'Ford-Probe',	11470,	'USA',	'Sporty',	30, 2,
'Ford-Tempo',	9483,	'USA',	'Compact',	24,  3,
'Ford-Thunderbird',	14980,	'USA',	'Medium',	23, 2,
'Honda-Accord',	12145,	'Japan',	'Compact',	26, 3,
'Honda-Civic',	6635,	'Japan',	'Small',	32,  5,
'Honda-Civic-CRX',	9410,	'Japan',	'Sporty',	33, 5,
'Honda-Prelude',	13945,	'Japan',	'Sporty',	27, 3,
'Mazda-626',	12459,	'Japan',	'Compact',	24, 4,
'Mazda-929',	23300,	'Japan',	'Medium',	21, 3,
'Mazda-MPV',	14944,	'Japan',	'Van',	19, 3,
'Mazda-Protege',	6599,	'Japan',	'Small',	32, 5,
'Mitsubishi-Galant',	10989,	'Japan',	'Compact',	25, 3,
'Mitsubishi-Sigma',	17879,	'Japan',	'Compact',	21, 2,
'Nissan-240SX',	13249,	'Japan',	'Sporty',	24, 3,
'Nissan-Maxima',	17899,	'Japan',	'Medium',	22, 1,
'Nissan-Sentra',	7399,	'Japan',	'Small',	33, 4,
'Nissan-Stanza',	11650,	'Japan',	'Compact',	21, 3,
'Pontiac-Grand-Am',	10565,	'USA',	'Compact',	23, 3,
'Subaru-Legacy',	11499,	'Japan',	'Compact',	23, 3,
'Subaru-Loyale',	9599,	'Japan',	'Small',	25, 4,
'Toyota-Camry',	11588,	'Japan',	'Compact',	27, 3,
'Toyota-Corolla',	8748,	'Japan',	'Small',	29, 5,
'Toyota-Cressida',	21498,	'Japan',	'Medium',	23, 1,
'Toyota-Tercel',	6488,	'Japan',	'Small',	35, 4),
nrow=35,ncol=6,byrow=T)
dat1=as.data.frame(dat1)
colnames(dat1)=c('Make', 'Price', 'Country',	'Type',  'Mileage',  'CustomerRating')
dat1$Price=as.numeric(as.character(dat1$Price))
dat1$Mileage=as.numeric(as.character(dat1$Mileage))
dat1$CustomerRating=as.numeric(as.character(dat1$CustomerRating))

fitlm=lm(Price~Mileage+CustomerRating+Country+Type,dat1)
summary(fitlm)
drop1(fitlm,test="F")

