import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';

function Square(props) {
    let selected = props.selected ? "selected" : "";

    return (
      <button className="square" id={selected} onClick={props.onClick}>
        {props.value}
      </button>
    );
}

class Board extends React.Component {

    renderSquare(i) {
        return (<Square 
                    value={this.props.squares[i]} 
                    onClick={() => this.props.onClick(i)} 
                    selected={i === this.props.selected}
                />);
    }

    render() {
        return (
            <div>
            <div className="board-row">
                {this.renderSquare(0)}
                {this.renderSquare(1)}
                {this.renderSquare(2)}
            </div>
            <div className="board-row">
                {this.renderSquare(3)}
                {this.renderSquare(4)}
                {this.renderSquare(5)}
            </div>
            <div className="board-row">
                {this.renderSquare(6)}
                {this.renderSquare(7)}
                {this.renderSquare(8)}
            </div>
            </div>
        );
    }
}

class Game extends React.Component {

    constructor(props) {
        super(props);
        this.state = {
            xIsNext: true,
            history: [{
                squares: Array(9).fill(null),
            }],
            selected: null,
            stepNumber: 0,
        };
    }

    handleClick(i) {
        
        const history = this.state.history.slice(0, this.state.stepNumber+1);
        const current = history[history.length - 1];
        const squares = current.squares.slice();

        if(calculateWinner(squares)) return;

        const turn = this.state.xIsNext ? 'X' : 'O';
        if(countVals(squares, turn) < 3) {
            if(squares[i]) return;

            squares[i] = turn;
            this.setState((state, props) => ({
                history: history.concat({
                    squares: squares
                }),
                xIsNext: !state.xIsNext,
                stepNumber: history.length
            }));
        } 
        else {
            if(this.state.selected === null) {
                if(squares[i] !== turn) return;
                if(squares[4] === turn && i !== 4) return;

                this.setState({
                    selected: i,
                });
            }
            else if(isValidMove(squares, this.state.selected, i)) {
                squares[this.state.selected] = null;
                squares[i] = turn;

                this.setState((state, props) => ({
                    history: history.concat({
                       squares: squares 
                    }),
                    xIsNext: !state.xIsNext,
                    selected: null,
                    stepNumber: history.length,
                }));
            }
            else if(squares[i] === turn && squares[4] !== turn) {
                this.setState({
                    selected: i,
                });
            }

        }
    }

    jumpTo(step) {
        this.setState({
            stepNumber: step,
            xIsNext: (step % 2) === 0,
        });
    }

    render() {
        const history = this.state.history;
        const current = history[this.state.stepNumber];
        const winner = calculateWinner(current.squares);
        let status;
        if(winner)
            status = `Winner: ${winner}`;
        else
            status = `Next player: ${this.state.xIsNext ? 'X' : 'O'}`;

        const moves = history.map((step, move) => {
            const desc = move ? 'Go to move #' + move : 'Go to game start';
            return (
                <li key={move}>
                    <button onClick={() => this.jumpTo(move)}>{desc}</button>
                </li>
            );
        });

        return (
            <div className="game">
                <div className="game-board">
                <Board 
                    squares={current.squares} 
                    onClick={(i) => this.handleClick(i)} 
                    selected={this.state.selected}
                />
                </div>
                <div className="game-info">
                <div>{status}</div>
                <div></div>
                <ol>{moves}</ol>
                </div>
            </div>
        );
    }
}

function calculateWinner(squares) {
    const lines = [
        [0, 1, 2],
        [3, 4, 5],
        [6, 7, 8],
        [0, 3, 6],
        [1, 4, 7],
        [2, 5, 8],
        [0, 4, 8],
        [2, 4, 6],
    ];
    for (let i = 0; i < lines.length; i++) {
        const [a, b, c] = lines[i];
        if (squares[a] && squares[a] === squares[b] && squares[a] === squares[c]) {
            return squares[a];
        }
    }
    return null;
}

function countVals(squares, turn) {
    let count = 0;
    for(let i = 0; i < squares.length; i++)
        if(squares[i] === turn)
            count++;

    return count;
}

function isValidMove(squares, from, to) {
    if(squares[to] != null) return false;

    if(from === 0 || from === 3 || from === 6)
        return to === from-3 || to === from-2 || to === from+1 || to === from+3 || to === from+4;
    if(from === 1 || from === 4 || from === 7)
        return to === from-4 || to === from-3 || to === from-2 || to === from-1 ||
               to === from+1 || to === from+2 || to === from+3 || to === from+4;
    return to === from+3 || to === from+2 || to === from-1 || to === from-3 || to === from-4;
}

// ========================================

ReactDOM.render(
    <Game />,
    document.getElementById('root')
);
