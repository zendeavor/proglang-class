# This is the only file you turn in, so do not modify the other files as part
# of your solution.

class MyTetris < Tetris

    def set_board
        @canvas = TetrisCanvas.new
        @board  = MyBoard.new(self)
        @canvas.place(@board.block_size * @board.num_rows + 3,
                      @board.block_size * @board.num_columns + 6,
                      24, 80)
        @board.draw
    end

    def key_bindings
        super
        @root.bind('c', proc { @board.cheat     })
        @root.bind('u', proc { @board.rotate180 })
    end

end

class MyPiece < Piece

    # The constant All_My_Pieces should be declared here
    All_My_Pieces =  Piece::All_Pieces + [
        rotations([[0, 0], [-1, 0], [-1, 1], [0, 1], [1, 1]]),
        [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]],
         [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
        rotations([[0, 0], [0, -1], [1, 0]])
    ]

    def self.next_piece (board, cheat = false)
        if cheat then
            MyPiece.new([[[0,0]]], board)
        else
            MyPiece.new(All_My_Pieces.sample, board)
        end
    end

end

class MyBoard < Board

    def initialize (game)
        super game
        @cheating      = false
        @current_block = MyPiece.next_piece(self)
    end

    def next_piece
        @current_block = MyPiece.next_piece(self, @cheating)
        @current_pos   = nil
        @cheating      = false
    end

    def store_current
        locations    = @current_block.current_rotation
        displacement = @current_block.position
            (0 .. locations.size - 1).each do |index|
            current = locations[index]
            @grid[current[1] + displacement[1]][current[0] + displacement[0]] = @current_pos[index]
        end
        remove_filled
        @delay = [@delay - 2, 80].max
    end

    def cheat
        if not @cheating and @score >= 100 then
            @score = @score - 100
            @cheating = true
        end
        draw
    end

    def rotate180
        rotate_clockwise
        rotate_clockwise
    end

end
