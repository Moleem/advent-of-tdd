package day05.model


case class MappingRange(from: Long, to: Long, delta: Long)

case class Mappings(
                     seeds: List[Long],
                     seedToSoilMap:            List[MappingRange],
                     soilToFertilizerMap:      List[MappingRange],
                     fertilizerToWaterMap:     List[MappingRange],
                     waterToLightMap:          List[MappingRange],
                     lightToTemperatureMap:    List[MappingRange],
                     temperatureToHumidityMap: List[MappingRange],
                     humidityToLocationMap:    List[MappingRange]
                   )
