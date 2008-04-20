package Chess::Rep;

use strict;

use POSIX;

our $VERSION = '0.1';

use constant {
    CASTLE_W_OO  => 1,
    CASTLE_W_OOO => 2,
    CASTLE_B_OO  => 4,
    CASTLE_B_OOO => 8,
};

use Exporter 'import';

our %EXPORT_TAGS = (
    castle => [qw( CASTLE_W_OO
                   CASTLE_W_OOO
                   CASTLE_B_OO
                   CASTLE_B_OOO )],
);

Exporter::export_ok_tags('castle');

use constant FEN_STANDARD => 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';

=head1 NAME

Chess::Rep - represent chess positions, generate list of legal moves, parse moves in various formats.

The name stands for "Chess Representation", basically meaning that
this module won't actually play chess -- it just helps you represent
the board and validate the moves according to the laws of chess.  It
also generates a set of all valid moves for the color to play.

=head1 SYNOPSIS

  my $pos = Chess::Rep->new;
  print $pos->get_fen;

  # use any decent notation to describe moves
  # the parser will read pretty much anything which isn't ambiguous

  $pos->go_move('e4');
  $pos->go_move('e7e5');
  $pos->go_move('Bc4');
  $pos->go_move('Nc8-C6');
  $pos->go_move('Qf3');
  $pos->go_move('d6');
  $pos->go_move('F3-F7');

  if ($pos->status->{check}) {
    print("CHECK\n");
  }

  if ($pos->status->{mate}) {
    print("MATE\n");
  }

  if ($pos->status->{stalemate}) {
    print("STALEMATE\n");
  }

  # reset position from FEN

  $pos->set_from_fen('r1b1k1nr/pp1ppppp/8/2pP4/3b4/8/PPP1PqPP/RNBQKBNR w KQkq - 0 1');
  my $status = $pos->status;

  my $moves = $status->{moves}; # there's only one move, E1-D2
  print Chess::Rep::get_field_id($moves->[0]{from}) . '-' .
        Chess::Rep::get_field_id($moves->[0]{to});

  print $status->{check};   # 1
  print $status->{mate};
  print $status->{stalemate};

=head1 REPRESENTATION

=head2 Pieces and colors

Pieces are represented as single letters, in the standard notation:

  P - pawn
  N - knight
  B - bishop
  R - rook
  Q - queen
  K - king

We use uppercase to represent white pieces and lowercase for black
pieces, as in standard FEN notation.

Whenever you need to deal with colors alone, 0 means black and 1 is
white.  For example the piece_color($p) function returns 0 for a black
piece and 1 for a white piece.

=head2 Position

The diagram is represented in an array, according to an idea
originally developed for Sargon [2].  It is imagined as a two
dimensional array of 12 rows and 10 columns (although we really use a
plain array and just map field indexes to row/cols as needed).  Using
a bigger board, we keep an "undef" value in the fields that are
off-board and it becomes very easy to generate moves.  Here's how it
looks like:

  * * * * * * * * * *
  * * * * * * * * * *
  * r n b q k b n r *
  * p p p p p p p p *
  * - - - - - - - - *
  * - - - - - - - - *
  * - - - - - - - - *
  * - - - - - - - - *
  * P P P P P P P P *
  * R N B Q K B N R *
  * * * * * * * * * *
  * * * * * * * * * *

The stars are "off-board fields" (undef-s).  The dashes are empty
squares (contain 0).

Now, let's see the array above, this time with field numbers (indexes)
instead of pieces.

    110   111   112   113   114   115   116   117   118   119
    100   101   102   103   104   105   106   107   108   109
        +-----------------------------------------------+
     90 |  91    92    93    94    95    96    97    98 |  99
     80 |  81    82    83    84    85    86    87    88 |  89
     70 |  71    72    73    74    75    76    77    78 |  79
     60 |  61    62    63    64    65    66    67    68 |  69
     50 |  51    52    53    54    55    56    57    58 |  59
     40 |  41    42    43    44    45    46    47    48 |  49
     30 |  31    32    33    34    35    36    37    38 |  39
     20 |  21    22    23    24    25    26    27    28 |  29
        +-----------------------------------------------+
     10    11    12    13    14    15    16    17    18    19
      0     1     2     3     4     5     6     7     8     9

The indexes that map to valid board fields are B<21..28>, B<31..38>,
... B<91..98>.  For these fields, our array will contain 0 if the
field is empty, or a piece character.  For offboard fields, our array
will contain B<undef>.  It's thus very easy to generate moves for
pieces.

For example, let's say that we have a N (knight) on field 31 ('a2' on
a chess board) and we need to generate a list of all fields where it
can possibly move.  All we have to do is to add some values to the
starting position (31) and check that the resulting field is in board.

  31 + 19 = 50  (off board)
  31 + 21 = 52  (valid)
  31 +  8 = 39  (off board)
  31 + 12 = 43  (valid)
  31 - 19 = 12  (off board)
  31 - 21 = 10  (off board)
  31 -  8 = 23  (valid)
  31 - 12 = 19  (off board)

Using simple arithmetic operations we determined the 3 fields that a
knight on a3 is allowed to move to.  Similar logic can be easily
applied for all piece types.

=head2 Some terms used in this doc

Following, when I refer to a field "index", I really mean an index in
that array, which can be 0..119.  Using get_index() you can compute an
index from a field ID.  By field ID I mean a field in standard
notation, i.e. 'e4' (case insensitive).

When I refer to row / col, I mean a number 0..7.  Field A1 corresponds
to row = 0 and col = 0, and has index 21.  Field H7 has row = 7, col =
7 and index 98.

Internally this object works with field indexes.  TODO: most functions
accept field ID-s too (or even row/col), which probably slows things
down because they have to check the form of the argument and determine
a correct index.  I should optimize this.

=cut

=head1 METHODS AND FUNCTIONS

=head3 new($fen)

Constructs a new object.  Pass a FEN string if you want to initialize
to a certain position.  Otherwise it will be initialized with the
standard starting position.

=cut

sub new {
    my ($class, $fen) = @_;
    my $self = {};
    bless $self, $class;
    $self->set_from_fen($fen || FEN_STANDARD);
    return $self;
}

=head2 reset()

Resets the object to standard start position.

=cut

sub reset {
    shift->set_from_fen(FEN_STANDARD);
}

=head3 set_from_fen($fen)

Reset this object to a position described in FEN notation.

=cut

sub set_from_fen {
    my ($self, $fen) = @_;
    $self->_reset;
    my @data = split(/\s+/, $fen);
    my ($board, $to_move, $castle, $enpa, $halfmove, $fullmove) = @data;
    my @board = reverse(split(/\//, $board));
    for my $row (0..7) {
        my $data = $board[$row];
        my $col = 0;
        while (length $data > 0) {
            my $p = substr($data, 0, 1, '');
            if ($p =~ /[pnbrqk]/i) {
                $self->set_piece_at(get_index($row, $col++), $p);
            } elsif ($p =~ /[1-8]/) {
                $col += $p;
            } else {
                die "Error parsing FEN position: $fen";
            }
        }
    }
    my $c = 0;
    $c |= CASTLE_W_OO  if index($castle, 'K') >= 0;
    $c |= CASTLE_W_OOO if index($castle, 'Q') >= 0;
    $c |= CASTLE_B_OO  if index($castle, 'k') >= 0;
    $c |= CASTLE_B_OOO if index($castle, 'q') >= 0;
    $self->{castle} = $c;
    if (lc $to_move eq 'w') {
        $self->{to_move} = 1;
    } elsif (lc $to_move eq 'b') {
        $self->{to_move} = 0;
    } else {
        $self->{to_move} = undef;
    }
    $self->{enpa} = $enpa ne '-' ? get_index($enpa) : 0;
    $self->{fullmove} = $fullmove;
    $self->{halfmove} = $halfmove;
    $self->_compute_valid_moves;
}

=head3 get_fen()

Returns the current position in standard FEN notation.

=cut

sub get_fen {
    my ($self) = @_;
    my @a;
    for (my $row = 8; --$row >= 0;) {
        my $str = '';
        my $empty = 0;
        for my $col (0..7) {
            my $p = $self->get_piece_at($row, $col);
            if ($p) {
                $str .= $empty
                  if $empty;
                $empty = 0;
                $str .= $p;
            } else {
                ++$empty;
            }
        }
        $str .= $empty
          if $empty;
        push @a, $str;
    }
    my $pos = join('/', @a);
    @a = ( $pos );
    $a[1] = $self->{to_move} ? 'w' : 'b';
    my $castle = $self->{castle};
    my $c = '';
    $c .= 'K' if $castle & CASTLE_W_OO;
    $c .= 'Q' if $castle & CASTLE_W_OOO;
    $c .= 'k' if $castle & CASTLE_B_OO;
    $c .= 'q' if $castle & CASTLE_B_OOO;
    $a[2] = $c || '-';
    $a[3] = $self->{enpa} ? lc get_field_id($self->{enpa}) : '-';
    # FIXME: ignoring "halfmove clock" and "fullmove number"
    $a[4] = $self->{halfmove};
    $a[5] = $self->{fullmove};
    return join(' ', @a);
}

=head3 status()

Returns the status of the current position.  The status is
automatically computed by an internal function --
_compute_valid_moves() -- and it's a hash as follows:

  {
    moves      => \@array_of_all_legal_moves,
    hash_moves => \%hash_of_all_legal_moves,
    type_moves => \%hash_of_moves_by_type_and_target_field,
    check      => 1 if king is in check, undef otherwise,
    mate       => 1 if position is mate, undef otherwise,
    stalemate  => 1 if position is stalemate, undef otherwise
  }

The last three are obvious -- simple boolean indicators that describe
the position state.  The first three are:

=head4 * B<moves>

An array of all the legal moves.  A move is represented as a hash
containing:

  {
    from  => $index_of_origin_field,
    to    => $index_of_target_field,
    piece => $id_of_the_moved_piece
  }

=head4 * B<hash_moves>

A hash table containing as keys all legal moves, in the form
"$from_index:$to_index".  For example, should E2-E4 be the single
legal move, then this hash would be:

  {
    '35-55' => 1
  }

=head4 * B<type_moves>

Again a hash table that maps target fields to piece types.  For
example, if you want to determine all white bishops that can move on
field C4 (index 58), you can do the following:

  my $a = $self->status->{type_moves}{58}{B};

@$a now contains the indexes of the fields that currently hold white
bishops that are allowed to move on C4.

This hash is mainly useful when we interpret standard algebraic
notation.

=cut

sub status {
    return shift->{status};
}

sub _reset {
    my ($self) = @_;
    my @a;
    for my $i (0..119) {
        my $m = $i % 10;
        $a[$i] = $i < 21 || $i > 98 || $m == 0 || $m == 9
          ? undef : 0;
    }
    $self->{pos} = \@a;
    $self->{castle} = CASTLE_W_OO | CASTLE_W_OOO | CASTLE_B_OO | CASTLE_B_OOO;
    $self->{to_move} = 1; # white
    $self->{enpa} = 0;
    $self->{halfmove} = 0;
    $self->{fullmove} = 0;
    $self->{status} = undef;
}

=head3 set_piece_at($where, $piece)

Sets the piece at the given position.  $where can be:

  - a full index conforming to our representation
  - a standard field ID (i.e. 'e2')

The following are equivalent:

  $self->set_piece_at(35, 'P');
  $self->set_piece_at('e2', 'P');

=cut

sub set_piece_at {
    my ($self, $index, $p) = @_;
    if ($index =~ /^[a-h]/oi) {
        $index = get_index($index);
    }
    my $old = $self->get_piece_at($index);
    $self->{pos}[$index] = $p;
    return $old;
}

=head3 get_piece_at($where, $col)

Returns the piece at the given position.  $where can be:

  - a full index conforming to our representation
  - a 0..7 row number (in which case $col is required)
  - a standard field ID (i.e. 'e2')

The following are equivalent:

  $self->get_piece_at('e2');
  $self->get_piece_at(35);
  $self->get_piece_at(1, 4);

If you call this function in array context, it will return the index
of the field as well; this is useful if you don't pass a computed
index:

  ($piece, $index) = $self->get_piece_at('e2');
  # now $piece is 'P' and $index is 35

=cut

sub get_piece_at {
    my ($self, $index, $col) = @_;
    if (defined $col) {
        $index = get_index($index, $col);
    } elsif ($index =~ /^[a-h]/oi) {
        $index = get_index($index);
    }
    my $p = $self->{pos}[$index];
    return ($p, $index)
      if wantarray;
    return $p;
}

=head3 to_move()

Returns (and optionally sets if you pass an argument) the color to
move.  Colors are 0 (black) or 1 (white).

=cut

sub to_move {
    my $self = shift;
    $self->{to_move} = shift
      if @_;
    return $self->{to_move};
}

=head3 go_move($move)

Updates the position with the given move.  The parser is very
forgiving; it understands a wide range of move formats:

  e4, e2e4, exf5, e:f5, e4xf5, e4f5, Nc3, b1c3, b1-c3,
  a8=Q, a7a8q#, a7-a8=q#, a8Q, etc.

After the move is executed, the position status is recomputed and you
can access it calling $self->status.  Also, the turn is changed
internally (see L<to_move()>).

This method returns a hash containing detailed information about this
move.  For example, for "axb8=Q" it will return:

  {
    from        => 'A7'
    from_index  => 81
    from_row    => 6
    from_col    => 0
    to          => 'B8'
    to_index    => 92
    to_row      => 7
    to_col      => 1
    piece       => 'P'
    promote     => 'Q'
    san         => 'axb8=Q'
  }

Of course, the exact same hash would be returned for "a7b8q",
"A7-b8=Q", "b8Q".  This method parses a move that can be given in a
variety of formats, and returns a canonical representation of it
(including a canonical SAN notation which should be understood by any
conformant parser on the planet).

=cut

sub go_move {
    my ($self, $move) = @_;
    my ($from, $from_index, $to, $to_index, $piece);

    my $color = $self->{to_move};
    my $col;
    my $row;
    my $promote;

    my $orig_move = $move;

    if ($move eq 'O-O') {
        $move = $color ? 'E1-G1' : 'E8-G8';
    } elsif ($move eq 'O-O-O') {
        $move = $color ? 'E1-C1' : 'E8-C8';
    }

    if ($move =~ s/^([PNBRQK])//) {
        $piece = $1;
    }

    if ($move =~ s/^([a-h][1-8])[:x-]?([a-h][1-8])//i) { # great, no ambiguities

        ($from, $to) = ($1, $2);

    } elsif ($move =~ s/^([a-h])[:x-]?([a-h][1-8])//i) {

        $col = ord(uc $1) - 65;
        $to = $2;

    } elsif ($move =~ s/^([1-8])[:x-]?([a-h][1-8])//i) {

        $row = ord($1) - 49;
        $to = $2;

    } elsif ($move =~ s/^[:x-]?([a-h][1-8])//i) {

        $to = $1;

    } else {

        die("Could not parse move: $orig_move");

    }

    if ($move =~ s/^=?([RNBQ])//i) {
        $promote = uc $1;
    }

    if (!$piece) {
        if (!$from) {
            $piece = 'P';
        } else {
            $piece = $self->get_piece_at($from);
            if (!$piece) {
                die("Illegal move: $orig_move (field $from is empty)");
            }
        }
    }

    my $is_pawn = lc $piece eq 'p';

    if (!$color) { # is black, make lowercase
        $piece = lc $piece;
    }

    if (!$to) {
        die("Can't parse move: $orig_move (missing target field)");
    }

    $to_index = get_index($to);

    # all moves that a piece of type $piece can make to field $to_index
    my $tpmove = $self->{status}{type_moves}{$to_index}{$piece};

    if (!$tpmove || !@$tpmove) {
        die("Illegal move: $orig_move (no piece '$piece' can move to $to)");
    }

    if (!$from) {
        if (@$tpmove == 1) {
            # unambiguous
            $from_index = $tpmove->[0];
        } else {
            foreach my $origin (@$tpmove) {
                my ($t_row, $t_col) = get_row_col($origin);
                if (defined($row) && $row == $t_row) {
                    $from_index = $origin;
                    last;
                } elsif (defined($col) && $col == $t_col) {
                    $from_index = $origin;
                    last;
                }
            }
        }
        if ($from_index) {
            $from = get_field_id($from_index);
        } else {
            die("Ambiguous move: $orig_move");
        }
    }

    if (!$from_index) {
        $from_index = get_index($from);
    }

    $from = uc $from;
    $to = uc $to;

    my ($from_row, $from_col) = get_row_col($from_index);
    my ($to_row, $to_col) = get_row_col($to_index);

    # execute move

    $self->{enpa} = 0;

    my $is_capture = 0;
    my $san;                    # compute canonical notation

  SPECIAL: {
        # 1. if it's castling, we have to move the rook
        $move = "$from-$to";
        if ($move eq 'E1-G1') {
            $san = 'O-O';
            $self->_move_piece(28, 26); last SPECIAL;
        } elsif ($move eq 'E8-G8') {
            $san = 'O-O';
            $self->_move_piece(98, 96); last SPECIAL;
        } elsif ($move eq 'E1-C1') {
            $san = 'O-O-O';
            $self->_move_piece(21, 24); last SPECIAL;
        } elsif ($move eq 'E8-C8') {
            $san = 'O-O-O';
            $self->_move_piece(91, 94); last SPECIAL;
        }

        # 2. is it en_passant?
        if ($is_pawn) {
            if ($from_col != $to_col && !$self->get_piece_at($to_index)) {
                $self->set_piece_at(get_index($from_row, $to_col), 0);
                $is_capture = 1;
                last SPECIAL;
            }
            if (abs($from_row - $to_row) == 2) {
                $self->{enpa} = get_index(($from_row + $to_row) / 2, $from_col);
            }
        }
    }

    {
        my $tmp = $self->_move_piece($from_index, $to_index, $promote);
        $is_capture ||= $tmp;
    }
    $self->{to_move} = 1 - $self->{to_move};

    if ($self->{to_move}) {
        ++$self->{fullmove};
    }

    if (!$is_pawn && !$is_capture) {
        ++$self->{halfmove};
    } else {
        $self->{halfmove} = 0;
    }

    my $status = $self->_compute_valid_moves;

    if (!$san) {
        $san = $is_pawn ? '' : uc $piece;

        my $len = ($is_capture && $is_pawn || @$tpmove > 1) ? 1 : 0;
        foreach my $origin (@$tpmove) {
            if ($origin != $from_index && $origin % 10 == $from_index % 10) {
                $len = 2;
                last;
            }
        }

        $san .= lc substr($from, 0, $len);
        $san .= 'x'
          if $is_capture;
        $san .= lc $to;
        $san .= "=$promote"
          if $promote;
        if ($status->{mate}) {
            $san .= '#';
        } elsif ($status->{check}) {
            $san .= '+';
        }
    }

    # _debug("$orig_move \t\t\t $san");

    return {
        from       => lc $from,
        from_index => $from_index,
        from_row   => $from_row,
        from_col   => $from_col,
        to         => lc $to,
        to_index   => $to_index,
        to_row     => $to_row,
        to_col     => $to_col,
        piece      => $piece,
        promote    => $promote,
        san        => $san,
    };
}

sub _move_piece {
    my ($self, $from, $to, $promote) = @_;
    my $p = $self->set_piece_at($from, 0);
    my $color = piece_color($p);
    my $lp = lc $p;
    if ($promote) {
        $p = $color ? uc $promote : lc $promote;
    }
    if ($lp eq 'k') {
        if ($color) {
            $self->{castle} = $self->{castle} | CASTLE_W_OOO ^ CASTLE_W_OOO;
            $self->{castle} = $self->{castle} | CASTLE_W_OO ^ CASTLE_W_OO;
        } else {
            $self->{castle} = $self->{castle} | CASTLE_B_OOO ^ CASTLE_B_OOO;
            $self->{castle} = $self->{castle} | CASTLE_B_OO ^ CASTLE_B_OO;
        }
    }
    if ($from == 21 || $to == 21) {
        $self->{castle} = $self->{castle} | CASTLE_W_OOO ^ CASTLE_W_OOO;
    } elsif ($from == 91 || $to == 91) {
        $self->{castle} = $self->{castle} | CASTLE_B_OOO ^ CASTLE_B_OOO;
    } elsif ($from == 28 || $to == 28) {
        $self->{castle} = $self->{castle} | CASTLE_W_OO ^ CASTLE_W_OO;
    } elsif ($from == 98 || $to == 98) {
        $self->{castle} = $self->{castle} | CASTLE_B_OO ^ CASTLE_B_OO;
    }
    $self->set_piece_at($to, $p);
}

sub _compute_valid_moves {
    my ($self) = @_;

    my @pieces;
    my $king;
    my $op_color = 1 - $self->{to_move};

    for my $row (0..7) {
        for my $col (0..7) {
            my ($p, $i) = $self->get_piece_at($row, $col);
            if ($p) {
                if (piece_color($p) == $self->{to_move}) {
                    push @pieces, {
                        from => $i,
                        piece => $p,
                    };
                    if (lc $p eq 'k') {
                        # remember king position
                        $king = $i;
                    }
                }
            }
        }
    }

    $self->{in_check} = $self->is_attacked($king, $op_color);

    my @all_moves;
    my %hash_moves;
    my %type_moves;

    foreach my $p (@pieces) {
        my $from = $p->{from};
        my $moves = $self->_get_allowed_moves($from);
        my $piece = $p->{piece};
        my $try_move = {
            from  => $from,
            piece => $piece,
        };
        my $is_king = $from == $king;
        my @valid_moves = grep {
            $try_move->{to} = $_;
            !$self->is_attacked($is_king ? $_ : $king, $op_color, $try_move);
        } @$moves;
        # _debug("Found moves for $piece");
        $p->{to} = \@valid_moves;
        push @all_moves, (map {
            my $to = $_;
            $hash_moves{"$from-$to"} = 1;
            my $a = ($type_moves{$to} ||= {});
            my $b = ($a->{$piece} ||= []);
            push @$b, $from;
            { from => $from, to => $to, piece => $piece }
        } @valid_moves);
    }

    return $self->{status} = {
        moves      => \@all_moves,
        hash_moves => \%hash_moves,
        type_moves => \%type_moves,
        check      => $self->{in_check},
        mate       => $self->{in_check} && !@all_moves,
        stalemate  => !$self->{in_check} && !@all_moves,
    };
}

=head3 is_attacked($index, $color, $try_move)

Checks if the field specified by $index is under attack by a piece of
the specified $color.

$try_move is optional; if passed it must be a hash of the following
form:

  { from  => $from_index,
    to    => $to_index,
    piece => $piece }

In this case, the method will take the given move into account.  This
is useful in order to test moves in _compute_valid_moves(), as we need
to filter out moves that leave the king in check.

=cut

sub is_attacked {
    my ($self, $i, $opponent_color, $try_move) = @_;

    # _debug("Checking if " . get_field_id($i) . " is attacked");

    $opponent_color = 1 - $self->{to_move}
      unless defined $opponent_color;

    my $test = sub {
        my ($type, $i) = @_;
        my $p;
        if ($try_move) {
            if ($i == $try_move->{from}) {
                $p = 0;
            } elsif ($i == $try_move->{to}) {
                $p = $try_move->{piece};
            } else {
                $p = $self->{pos}[$i];
            }
        } else {
            $p = $self->{pos}[$i];
        }
        return 1 unless defined $p;
        if ($p && piece_color($p) == $opponent_color && index($type, lc $p) >= 0) {
            die 1;
        }
        return $p;
    };

    eval {

        # check pawns
        # _debug("... checking opponent pawns");
        if ($opponent_color) {
            $test->('p', $i - 9);
            $test->('p', $i - 11);
        } else {
            $test->('p', $i + 9);
            $test->('p', $i + 11);
        }

        # check knights
        # _debug("... checking opponent knights");
        for my $step (19, 21, 8, 12, -19, -21, -8, -12) {
            $test->('n', $i + $step);
        }

        # check bishops or queens
        # _debug("... checking opponent bishops");
        for my $step (11, 9, -11, -9) {
            my $j = $i;
            do { $j += $step }
              while (!$test->('bq', $j));
        }

        # check rooks or queens
        # _debug("... checking opponent rooks or queens");
        for my $step (1, 10, -1, -10) {
            my $j = $i;
            do { $j += $step }
              while (!$test->('rq', $j));
        }

        # _debug("... checking opponent king");
        for my $step (9, 10, 11, -1, 1, -9, -10, -11) {
            $test->('k', $i + $step);
        }

    };

    return $@ ? 1 : 0;
}

sub _get_allowed_moves {
    my ($self, $index) = @_;
    my $p = $self->get_piece_at($index);
    my $color = piece_color($p);
    $p = uc $p;
    my $method = "_get_allowed_${p}_moves";
    return $self->$method($index);
}

sub _add_if_valid {
    my ($self, $moves, $from, $to) = @_;

    my $what = $self->get_piece_at($to);

    return
      unless defined $what;           # off-board position

    my $p = $self->get_piece_at($from);
    my $color = piece_color($p);
    $p = lc $p;

    if ($p eq 'k' && $self->is_attacked($to)) {
        return undef;
    }

    if (!$what) {
        if ($p eq 'p') {
            if (abs($from % 10 - $to % 10) == 1) {
                # _debug("En passant ($self->{enpa} | $to)");
                if ($to == $self->{enpa}) { # check en passant
                    # _debug("Adding en-passant: $p " . get_field_id($from) . "-" . get_field_id($to));
                    push @$moves, $to; # allowed
                    return $to;
                }
                return undef; # must take to move this way
            }
        }
        # _debug("Adding move $p " . get_field_id($from) . "-" . get_field_id($to));
        push @$moves, $to;
        return $to;
    }

    if (piece_color($what) != $color) {
        if ($p eq 'p' && $from % 10 == $to % 10) {
            return undef;   # pawns can't take this way
        }
        # _debug("Adding capture: $p " . get_field_id($from) . "-" . get_field_id($to));
        push @$moves, $to;
        return $to;
    }

    return undef;
}

sub _get_allowed_P_moves {
    my ($self, $index, $moves) = @_;
    $moves ||= [];
    my $color = $self->piece_color($index);
    my $step = $color ? 10 : -10;
    my $not_moved = $color
      ? ($index >= 31 && $index <= 38)
        : ($index >= 81 && $index <= 88);
    if ($self->_add_if_valid($moves, $index, $index + $step) && $not_moved) {
        $self->_add_if_valid($moves, $index, $index + 2 * $step);
    }
    $self->_add_if_valid($moves, $index, $index + ($color ? 11 : -9));
    $self->_add_if_valid($moves, $index, $index + ($color ? 9 : -11));
    return $moves;
}

sub _get_allowed_N_moves {
    my ($self, $index, $moves) = @_;
    $moves ||= [];
    for my $step (19, 21, 8, 12, -19, -21, -8, -12) {
        $self->_add_if_valid($moves, $index, $index + $step);
    }
    return $moves;
}

sub _get_allowed_R_moves {
    my ($self, $index, $moves) = @_;
    $moves ||= [];
    for my $step (1, 10, -1, -10) {
        my $i = $index;
        while ($self->_add_if_valid($moves, $index, $i += $step)) {
            last if $self->get_piece_at($i);
        }
    }
    return $moves;
}

sub _get_allowed_B_moves {
    my ($self, $index, $moves) = @_;
    $moves ||= [];
    for my $step (11, 9, -11, -9) {
        my $i = $index;
        while ($self->_add_if_valid($moves, $index, $i += $step)) {
            last if $self->get_piece_at($i);
        }
    }
    return $moves;
}

sub _get_allowed_Q_moves {
    my ($self, $index, $moves) = @_;
    $moves ||= [];
    $self->_get_allowed_R_moves($index, $moves);
    $self->_get_allowed_B_moves($index, $moves);
    return $moves;
}

sub _get_allowed_K_moves {
    my ($self, $index, $moves) = @_;
    $moves ||= [];

    for my $step (9, 10, 11, -9, -10, -11) {
        $self->_add_if_valid($moves, $index, $index + $step);
    }

    my $color = $self->piece_color($index);

    if ($self->_add_if_valid($moves, $index, $index + 1) &&
          !$self->{in_check} && $self->can_castle($color, 0) &&
            !$self->get_piece_at($index + 1) &&
              !$self->get_piece_at($index + 2)) {
        # kingside castling possible
        $self->_add_if_valid($moves, $index, $index + 2);
    }

    if ($self->_add_if_valid($moves, $index, $index - 1) &&
          !$self->{in_check} && $self->can_castle($color, 1) &&
            !$self->get_piece_at($index - 1) &&
              !$self->get_piece_at($index - 2) &&
                !$self->get_piece_at($index - 3)) {
        # queenside castling possible
        $self->_add_if_valid($moves, $index, $index - 2);
    }

    return $moves;
}

=head3 can_castle($color, $ooo)

Return true if the given $color can castle kingside (if $ooo is false)
or queenside (if you pass $ooo true).

=cut

sub can_castle {
    my ($self, $color, $ooo) = @_;
    if ($color) {
        return $self->{castle} & ($ooo ? CASTLE_W_OOO : CASTLE_W_OO);
    } else {
        return $self->{castle} & ($ooo ? CASTLE_B_OOO : CASTLE_B_OO);
    }
}

=head3 piece_color($piece)

You can call this both as an object method, or standalone.  It returns
the color of the specified piece.  Example:

  piece_color('P') --> 1
  piece_color('k') --> 0
  $self->piece_color('e2') --> 1  (in standard position)

If you call it as a method, the argument must be a field specifier
(either full index or field ID) rather than a piece.

=cut

sub piece_color {
    my $p = shift;
    $p = $p->get_piece_at(shift)
      if ref $p;
    return ord($p) < 97 ? 1 : 0;
}

=head3 get_index($row, $col)

Static function.  Computes the full index for the given $row and $col
(which must be in 0..7).

Additionally, you can pass a field ID instead (and omit $col).

Examples:

  get_index(2, 4) --> 45
  get_index('e3') --> 45

=cut

sub get_index {
    my ($row, $col) = @_;
    ($row, $col) = get_row_col($row)
      unless defined $col;
    return $row * 10 + $col + 21;
}

=head3 get_field_id($index)

Returns the ID of the field specified by the given index.

  get_field_id(45) --> 'e3'
  get_field_id('f4') --> 'f4' (quite pointless)

=cut

sub get_field_id {
    my ($row, $col) = @_;
    ($row, $col) = get_row_col($row)
      unless defined $col;
    return pack('CC', $col + 65, $row + 49);
}

=head3 get_row_col($where)

Returns a list of two values -- the $row and $col of the specified
field.  They are in 0..7.

  get_row_col('e3') --> (2, 4)
  get_row_col(45) --> (2, 4)

=cut

sub get_row_col {
    my ($id) = @_;
    if ($id =~ /^[a-h]/oi) {
        my ($col, $row) = unpack('CC', uc $id);
        return (
            $row - 49,
            $col - 65,
        );
    } else {
        $id -= 21;
        return (
            POSIX::floor($id / 10),
            $id % 10,
        );
    }
}

=head3 dump_pos()

Returns a string with the current position (in a form more readable
than standard FEN).  It's only useful for debugging.

=cut

sub dump_pos {
    my ($self) = @_;
    my $fen = $self->get_fen;
    my @a = split(/ /, $fen);
    $fen = shift @a;
    $fen =~ s/([1-8])/' 'x$1/ge;
    $fen =~ s/\//\n/g;
    return $fen;
}

sub _debug {
    print STDERR join(' / ', @_), "\n";
}

=head1 LINKS

 [1] SAN ("Standard Algebraic Notation") is the most popular notation
     for chess moves.

     http://en.wikipedia.org/wiki/Algebraic_chess_notation

 [2] Ideas for representing a chess board in memory.

     http://www.atariarchives.org/deli/computer_chess.php

=head1 AUTHOR

Mihai Bazon, <mihai.bazon@gmail.com>
    http://www.dynarchlib.com/
    http://www.bazon.net/mishoo/

This module was developed for Dynarch Chess --
L<http://chess.dynarch.com/en.html>

=head1 COPYRIGHT

Copyright (c) Mihai Bazon 2008.  All rights reserved.

This module is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT
WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER
PARTIES PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
SOFTWARE IS WITH YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME
THE COST OF ALL NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE LIABLE
TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE
SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
DAMAGES.

=cut

1;
