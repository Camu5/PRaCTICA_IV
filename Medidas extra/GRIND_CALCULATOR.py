def calcular_area_por_point(ancho_nueva_imagen=1250, alto_nueva_imagen=900, ancho_original=1002, alto_original=900, area_por_point_original=45090):
    # Calcular el área total de la nueva imagen
    area_total_nueva_imagen = ancho_nueva_imagen * alto_nueva_imagen
    
    # Calcular el área total de la imagen original
    area_total_imagen_original = ancho_original * alto_original
    
    # Calcular la nueva área por point usando la proporción
    area_por_point_nueva = (area_total_nueva_imagen / area_total_imagen_original) * area_por_point_original
    
    return area_por_point_nueva

# Maincore:
print("Calculadora de cuadrícula para ImageJ")
ancho_nueva_imagen = int(input("Introduce el ancho de la imagen (px): "))
alto_nueva_imagen = int(input("Introduce el alto de la imagen (px): "))
area_nueva = calcular_area_por_point(ancho_nueva_imagen, alto_nueva_imagen)
print(f"El área per point para la nueva imagen es: {area_nueva:.2f} píxeles²")
