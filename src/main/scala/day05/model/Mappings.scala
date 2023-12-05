package day05.model

case class Mappings(
                     seeds: List[Int],
                     seedToSoilMap: Map[Int, Int],
                     soilToFertilizerMap: Map[Int, Int],
                     fertilizerToWaterMap: Map[Int, Int],
                     waterToLightMap: Map[Int, Int],
                     lightToTemperatureMap: Map[Int, Int],
                     temperatureToHumidityMap: Map[Int, Int],
                     humidityToLocationMap: Map[Int, Int]
                   )
