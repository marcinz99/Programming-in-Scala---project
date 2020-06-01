package src{
import scala.util.control.Breaks.{break, breakable}

class Complex(val rl: Double, val im: Double){
    def add(other: Complex): Complex = {
        new Complex(rl + other.rl, im + other.im)
    }
    def mult(other: Complex): Complex = {
        new Complex(rl * other.rl - im * other.im, rl * other.im + im * other.rl)
    }
    def abs(): Double = {
        rl * rl + im * im
    }
}

class Vector2D(val x: Double, val y: Double){
    def subtract(other: Vector2D): Vector2D = {
        new Vector2D(x - other.x, y - other.y)
    }
}

class Parameters(val width_px: Int, val height_px: Int, val iters: Int,
                 val v_bottom_left: Vector2D, val v_upper_right: Vector2D){
    def transform(up: Double, down: Double, left: Double, right: Double): Parameters = {
        val width:  Double = v_upper_right.x - v_bottom_left.x
        val height: Double = v_upper_right.y - v_bottom_left.y
        new Parameters(
            width_px,
            height_px,
            iters,
            new Vector2D(v_upper_right.x - left * width, v_upper_right.y - down * height),
            new Vector2D(v_bottom_left.x + right * width, v_bottom_left.y + up * height)
        )
    }
    def move(vertical: Double, horizontal: Double): Parameters = {
        val width:  Double = v_upper_right.x - v_bottom_left.x
        val height: Double = v_upper_right.y - v_bottom_left.y
        new Parameters(
            width_px,
            height_px,
            iters,
            new Vector2D(v_bottom_left.x + horizontal * width,
                         v_bottom_left.y + vertical * height),
            new Vector2D(v_upper_right.x + horizontal * width,
                         v_upper_right.y + vertical * height),
        )
    }
}

object Printer{
    def print(params: Parameters): Unit = {
        val width_px: Int = params.width_px
        val height_px: Int = params.height_px
        val iters: Int = params.iters
        
        val v_bottom_left: Vector2D = params.v_bottom_left
        val v_upper_right: Vector2D = params.v_upper_right
        
        val breadth = v_upper_right.subtract(v_bottom_left)
        var line: String = ""
        
        val UL_corner: String = f"[${v_bottom_left.x}%e ${v_upper_right.y}%e]"
        val BR_corner: String = f"[${v_upper_right.x}%e ${v_bottom_left.y}%e]"
        val UL_compl: Int = width_px - UL_corner.length
        val BR_compl: Int = width_px - BR_corner.length
        
        val upper_bar: String = "+" ++
                                UL_corner ++
                                (for(i <-(0 until UL_compl).toList) yield('-')).mkString ++
                                "+"
        val bottom_bar: String ="+" ++
                                (for(i <-(0 until BR_compl).toList) yield('-')).mkString ++
                                BR_corner ++
                                "+"
        
        println(upper_bar)
        
        for(j <- 0 until height_px){
            line = line.concat("|")
            
            for(k <- 0 until width_px){
                val starting_point: Complex = new Complex(
                    v_bottom_left.x + (breadth.x * k).toDouble / (width_px-1),
                    v_upper_right.y - (breadth.y * j).toDouble / (height_px-1)
                )
                var temp: Complex = new Complex(0.0, 0.0)
                var successful_iters: Int = 0

                breakable{
                    for(n <- 0 until iters){
                        temp = starting_point.add(temp.mult(temp))
                        if(temp.abs < 4) successful_iters += 1;
                        else break
                    }
                }
                val success_rate: Double = successful_iters.toDouble / iters;
                
                if     (success_rate > 0.99) line = line.concat("@")
                else if(success_rate > 0.80) line = line.concat("O")
                else if(success_rate > 0.65) line = line.concat("o")
                else if(success_rate > 0.55) line = line.concat(";")
                else if(success_rate > 0.35) line = line.concat(",")
                else if(success_rate > 0.26) line = line.concat(".")
                else                         line = line.concat(" ")
            }
            line = line.concat("|")
            println(line)
            line = ""
        }
        println(bottom_bar)
    }
}

object ParamParser{
    def parse(str: String): Parameters = {
        var width_px: Int = 77
        var height_px: Int = 39
        var iters: Int = 35
        var BL_x: Double = -2.0
        var BL_y: Double = -1.0
        var UR_x: Double = 1.0
        var UR_y: Double = 1.0
        
        for(i <- str.linesIterator){
            val pair: Array[String] = i.split(" ")
            if     (pair(1) == "width_px")      width_px = pair(0).toInt
            else if(pair(1) == "height_px")     height_px = pair(0).toInt
            else if(pair(1) == "iters")         iters = pair(0).toInt
            else if(pair(1) == "bottom_left_x") BL_x = pair(0).toDouble
            else if(pair(1) == "bottom_left_y") BL_y = pair(0).toDouble
            else if(pair(1) == "upper_right_x") UR_x = pair(0).toDouble
            else if(pair(1) == "upper_right_y") UR_y = pair(0).toDouble
        }
        
        val v_bottom_left: Vector2D = new Vector2D(BL_x, BL_y)
        val v_upper_right: Vector2D = new Vector2D(UR_x, UR_y)
        
        new Parameters(width_px, height_px, iters, v_bottom_left, v_upper_right)
    }
}
}

object Appl{
    def main(args: Array[String]): Unit = {
        val default_params: String = "params\\parameters.txt"
        val file = scala.io.Source.fromFile(default_params)
        val file_content = file.mkString
        file.close
        
        var params: src.Parameters = src.ParamParser.parse(file_content)
        
        var cond: Boolean = true
        while(cond){
            src.Printer.print(params)
            print("$> ")
            val cmd: String = scala.io.StdIn.readLine()
            
            cmd match {
                case "quit"    => cond = false
                case "N up"    => params = params.transform(1.2, 1, 1, 1)
                case "N down"  => params = params.transform(0.8, 1, 1, 1)
                case "E left"  => params = params.transform(1, 1, 1, 0.8)
                case "E right" => params = params.transform(1, 1, 1, 1.2)
                case "S up"    => params = params.transform(1, 0.8, 1, 1)
                case "S down"  => params = params.transform(1, 1.2, 1, 1)
                case "W left"  => params = params.transform(1, 1, 1.2, 1)
                case "W right" => params = params.transform(1, 1, 0.8, 1)
                case "left"    => params = params.move(0, -0.2)
                case "right"   => params = params.move(0, 0.2)
                case "up"      => params = params.move(0.2, 0)
                case "down"    => params = params.move(-0.2, 0)
                case _         => 0
            }
        }
    }
}