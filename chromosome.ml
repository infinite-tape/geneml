
open Util
open Bigarray

class binary_chromosome length =
object(self)
  val mutable chrom_string = Array1.create int8_unsigned c_layout length
  val mutable score = 0.0
  val chrom_length = length

  method get_length = chrom_length
  method get_score = score
  method set_score new_score = score <- new_score
  method get_chrom_string = chrom_string
  method set_chrom_string s = chrom_string <- s
  method print_gene_string =
    for i = 0 to chrom_length - 1 do
      print_int chrom_string.{i}
    done
      
  method randomize =
    let random_gene () = if Random.bool () then 1 else 0 in 
      for i=0 to chrom_length - 1 do chrom_string.{i} <- random_gene () done;
end
