
object Main {

  // val input: List[String] = 
  //   """ULL
  //     |RRDDD
  //     |LURDL
  //     |UUUUD
  //     """.stripMargin.linesIterator.toList

  val input = 
    """DLUUULUDLRDDLLLUDULLULLRUURURLUULDUUUDLDDRUDLUULLRLDDURURDDRDRDLDURRURDLDUURULDDULDRDDLDLDLRDRUURLDLUDDDURULRLLLLRLULLUDRDLDUURDURULULULRLULLLULURLRDRDDDDDDDLRLULUULLULURLLDLRLUDULLDLLURUDDLDULDLULDDRLRLRDDLRURLLLURRLDURRDLLUUUUDRURUULRLDRRULLRUDLDRLUDRDRDRRDDURURRDRDRUDURDLUDRUDLRRULDLRDDRURDDUUDLDRDULDDRRURLLULRDRURLRLDLLLUULUUDLUDLDRRRRDUURULDUDUDRLDLLULLLRDDDDDLRDDLLUULLRRRDURLRURDURURLUDRRLRURDRDRRRRULUDLDRDULULRUDULLLUDRRLRLURDDURULDUUDULLURUULRDRDULRUUUDURURDDRRUDURRLRDRULRUUU
      |LDRURRUUUULDRDDDLLULDRUDDRLLDLDRDLRUDDDLDDULULULLRULDUDRRDLRUURURDRURURDLLRUURDUUDRLDURDRDLRRURURDUUUURUURRLLLDRDUURRRRURULUUUDLUDDRUURRLDULRDULRRRRUDURRLURULRURRDRDLLDRRDUDRDURLDDRURULDRURUDDURDLLLUURRLDRULLURDRDRLDRRURRLRRRDDDDLUDLUDLLDURDURRDUDDLUDLRULRRRDRDDLUDRDURDRDDUURDULRRULDLDLLUDRDDUDUULUDURDRLDURLRRDLDDLURUDRLDUURLLRLUDLLRLDDUDLLLRRRLDLUULLUDRUUDRLDUUUDUURLRDDDDRRDRLDDRDLUDRULDDDRDUULLUUUUULDULRLLLRLLDULRDUDDRDDLRRLRDDULLDURRRURDDUDUDDRLURRLUUUULLDRDULUUDRDULDLLUDLURDLLURRDLUULURRULRLURRRRRUURDDURLRLLDDLRRDUUURDRDUDRDDDLLDDRDRRRLURRDUULULULULRRURDDLDDLLLRUDDDDDDLLLRDULURULLRLRDRR
      |DDRLLLDLRRURRDLDDRUURRURRLRRRRUURUURDLURRRDDLRUDRURLUURLLRRLRLURLURURDULLLLDLRURULUUDURRLULRDRDRRDDLLULRLUDLUUUDRLLRRURRLDULDDLRRLUUUUDDLRLDRLRRDRDLDDURDDRDDLDLURLRRRDDUDLLRLRLURRRRULLULLLLDRLDULDLLDULRLDRDLDDRRDDDDRUDRLLURULRLDDLLRRURURDDRLLLULLULDDRDLDDDLRLLDRLDRUURRULURDDRLULLDUURRULURUUDULLRUDDRRLLDLLRDRUDDDDLLLDDDLLUUUULLDUUURULRUUDUUUDDLDURLDRDRRLLUDULDLUDRLLLDRRRULUUDDURUDRLUDDRRLLDUDUURDDRURLUURDURURURRUUDUDDLLLDRRRURURRURDLRULLDUDRLRLLRUDRUDLR
      |RRRDRLRURLRRLUURDRLDUURURLRDRRUDLLUUDURULLUURDLLDRRLURRUDUUDRRURLRRDULLDDLRRRUDUUDUUDLDDDLUUDLDULDDULLDUUUUDDUUDUDULLDDURRDLRRUDUDLRDUULDULRURRRLDLLURUDLDDDRRLRDURDLRRLLLRUDLUDRLLLRLLRRURUDLUDURLDRLRUDLRUULDRULLRLDRDRRLDDDURRRUDDDUDRRDRLDDRDRLLRLLRDLRDUDURURRLLULRDRLRDDRUULRDDRLULDLULURDLRUDRRDDDLDULULRDDRUDRLRDDRLDRDDRRRDUURDRLLDDUULRLLLULLDRDUDRRLUUURLDULUUURULLRLUDLDDLRRDLLRDDLRDRUUDURDDLLLDUUULUUDLULDUDULDRLRUDDURLDDRRRDLURRLLRRRUDDLDDRURDUULRUURDRRURURRRUUDUDULUDLUDLLLUUUULRLLRRRRDUDRRDRUDURLUDDLDRDLDDRULLRRULDURUL
      |DLLLRDDURDULRRLULURRDULDLUDLURDDURRLLRRLLULRDLDRDULRLLRDRUUULURRRLLRLDDDRDRRULDRRLLLLDLUULRRRURDDRULLULDDDLULRLRRRUDRURULUDDRULDUDRLDRRLURULRUULLLRUURDURLLULUURUULUUDLUDLRRULLLRRLRURDRRURDRULRURRUDUDDDRDDULDLURUDRDURLDLDLUDURLLRUULLURLDDDURDULRLUUUDLLRRLLUURRDUUDUUDUURURDRRRRRRRRRUDULDLULURUDUURDDULDUDDRDDRDRLRUUUUDLDLRDUURRLRUUDDDDURLRRULURDUUDLUUDUUURUUDRURDRDDDDULRLLRURLRLRDDLRUULLULULRRURURDDUULRDRRDRDLRDRRLDUDDULLDRUDDRRRD
      |""".stripMargin.linesIterator.toList

  val parsedInput: List[List[Dir]] = input.map(line => line.map(Dir.parse).toList)

  def main(args:Array[String]) = println(Part1.code)

  object Part1 {
    val keys = Seq(
      Seq('1','2','3'),
      Seq('4','5','6'),
      Seq('7','8','9')
    )
    val validKeys = "123456789".toSet

    val initialState = KeyPadState(KeyPad(keys,validKeys), Pos(1,1))

    def code = getCode(initialState, parsedInput)
  }

  def getCode(state0:KeyPadState, dirLines0:List[List[Dir]]): List[Char] = {
    def loop(dirLines:List[List[Dir]], state:KeyPadState, code:List[Option[Char]]): List[Option[Char]] =
      dirLines match {
        case Nil => code
        case currDirLine :: restDirLines => 
          val newState = state.moveAll(currDirLine)
          loop(restDirLines, newState, newState.key :: code)
      }
    val code = loop(dirLines0, state0, Nil).reverse
    assert(code.forall(_.isDefined))
    code.flatten
  }
}

sealed trait Dir
case object U extends Dir
case object D extends Dir
case object L extends Dir
case object R extends Dir

object Dir {
  def parse(d:Char): Dir =
    d match {
      case 'U' => U
      case 'D' => D
      case 'L' => L
      case 'R' => R
    }
}

case class Pos(r:Int, c:Int) {
  def move(dir: Dir) =
    dir match {
      case U => this.copy(r = r - 1)
      case D => this.copy(r = r + 1)
      case L => this.copy(c = c - 1)
      case R => this.copy(c = c + 1)
    }
}

case class KeyPad(val keys:Seq[Seq[Char]], validKeys: Set[Char]) {
  assert(keys.map(_.size).distinct.size == 1)
  def keyAt(pos: Pos): Option[Char] =
    for {
      row <- keys.lift(pos.r)
      key <- row.lift(pos.c)
      if validKeys.contains(key)
    } yield key

  // def keyAt(pos: Pos): Option[Char] =
  //   keys
  //     .lift(pos.r)
  //     .flatMap(_.lift(pos.c))
  //     .filter(validKeys.contains)
}

case class KeyPadState(keyPad:KeyPad, pos:Pos){
  def valid = key.isDefined
  def key = keyPad.keyAt(pos)

  def moveAll(dirs: List[Dir]) = {
    def loop(state:KeyPadState, dirsToProcess: List[Dir]): KeyPadState =
      dirsToProcess match {
        case Nil =>
          state
        case dir :: rest =>
          loop(state.move(dir), rest)
      }

    loop(this, dirs)
  }

  def move(dir: Dir): KeyPadState = {
    val nextState = this.copy(pos = pos.move(dir))
    if (nextState.valid)
      nextState
    else
      this
  }

  // def move(dir: Dir): KeyPadState =
  //   Some(this.copy(pos = pos.move(dir)))
  //     .filter(_.isValid)
  //     .getOrElse(this)

}
