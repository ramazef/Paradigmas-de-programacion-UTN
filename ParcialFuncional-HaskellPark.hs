--Link: https://www.utnianos.com.ar/foro/attachment.php?aid=25115

module Library where
import PdePreludat

data Parque = Parque {
atracciones :: [Atraccion]
}

data Atraccion = Atraccion{
nombre :: String,
alturaMinimaCm :: Number,
duracionMinutos :: Number,
opiniones :: [String],
mantenimiento :: Bool,
reparaciones :: [Reparacion]
}

data Reparacion = Reparacion {
    duracion :: Number,
    trabajo :: Trabajo
}
type Trabajo = Atraccion -> Atraccion

puntaje :: Atraccion -> Number
puntaje atraccion | duracionMinutos atraccion > 10 = 100
                      | length (reparaciones atraccion) < 3 = (10 * length (nombre atraccion)) + (2 * length (opiniones atraccion))
                      |otherwise = 10 * alturaMinimaCm atraccion

aplicarTrabajo :: Trabajo -> Atraccion -> Atraccion
aplicarTrabajo trabajo atraccion = chequearMantenimiento(eliminarUltimaReparacion(trabajo atraccion))
    where 
        eliminarUltimaReparacion atraccion = atraccion {reparaciones = init (reparaciones atraccion)}
        chequearMantenimiento atraccion | length (reparaciones atraccion) ==0 = atraccion{mantenimiento = False}
                                        | otherwise = atraccion {mantenimiento = True}
    

ajusteDeTornilleria :: Number->Trabajo
ajusteDeTornilleria numeroTornillos atraccion | (duracionMinutos atraccion + numeroTornillos) >= 10 = atraccion {duracionMinutos = 10}
                                              | (duracionMinutos atraccion + numeroTornillos) < 10 = atraccion {duracionMinutos = duracionMinutos atraccion + numeroTornillos}

engrase :: Number -> Trabajo
engrase gramosGrasa atraccion = (agregarOpinion(aumentarAlturaMinima gramosGrasa atraccion))
    where  
        aumentarAlturaMinima gramosGrasa atraccion = atraccion {alturaMinimaCm = alturaMinimaCm atraccion + (0.1 * gramosGrasa)}
        agregarOpinion atraccion = atraccion {opiniones = opiniones atraccion ++ ["para valientes"]}

mantenimientoElectrico :: Trabajo
mantenimientoElectrico atraccion = atraccion {opiniones = soloPrimeras3 atraccion} -- O {opiniones = take 3 (opiniones atraccion)}
    where 
        soloPrimeras3 atraccion = take 3 (opiniones atraccion)

mantenimientoBasico :: Trabajo
mantenimientoBasico atraccion = (engrase 10 (ajusteDeTornilleria 8 atraccion))

meDaMiedito :: Atraccion -> Bool
meDaMiedito atraccion = any ((>4) . duracion) (reparaciones atraccion)

acaCerramos :: Atraccion -> Bool
acaCerramos atraccion = sum (map duracion (reparaciones atraccion)) == 7

disneyNoEsistis :: Parque -> Bool
disneyNoEsistis parque = not (any (not . null . reparaciones) (filter ((>5) . length . nombre) (atracciones parque)))

reparacionesPeolas :: Atraccion -> Bool
reparacionesPeolas atraccion = 
    auxiliar atraccion (reparaciones atraccion)

auxiliar atraccion [] = True

auxiliar atraccion [reparacion] = puntaje ((trabajo reparacion) atraccion) > puntaje atraccion

auxiliar atraccion (reparacion1:reparacion2:resto) =
    puntaje atraccion2 > puntaje atraccion1
    &&
    auxiliar atraccion1 (reparacion2:resto)
    where
        atraccion1 = trabajo reparacion1 atraccion
        atraccion2 = trabajo reparacion2 atraccion1

realizarTrabajosPendientes :: Trabajo
realizarTrabajosPendientes atraccion = foldl (flip aplicarTrabajo) atraccion (accederTrabajos atraccion) 
accederTrabajos atraccion = map trabajo (reparaciones atraccion)