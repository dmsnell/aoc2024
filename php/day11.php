<?php

function rule( $stone ) {
    if ( '0' === $stone ) {
        return '1';
    }

    $len = strlen( $stone );
    if ( $len % 2 == 0 ) {
        $left  = substr( $stone, 0, $len / 2 );
        $right = substr( $stone, $len / 2 );
        $right = ltrim( $right, '0' );
        $right = '' === $right ? '0' : $right;

        echo "splitting!\n\n  s = {$stone}\n  l = {$left}\n  r = {$right}\n\n";
        return $left . ' ' . $right;
    }

    return (string) ( intval( $stone ) * 2024 );
}

function pp( $stones ) {
    echo "\e[33m" . implode( ' ', $stones ) . "\e[m\n";
}

function p2() {
    $input = file_get_contents( __DIR__ . '/../priv/day11_a.txt' );
    $stones = trim( $input );
    echo "\e[90mRound \e[33m0\e[90m: \e[2;3;33m{$stones}\e[m\n";
    $budget = 15000;

    $cache  = [];
    $was_at = 0;

    for ( $i = 1; $i <= 5; $i++ ) {
        $end = strlen( $stones );
        $next_stones = '';
        $next_start_at = 0;

        find_next_key:
        $key      = '';
        $next_key = '';
        $key_end  = $next_start_at;
        while ( $next_start_at < $end ) {
            if ( --$budget < 0 ) {
                exit(0);
            }

            $next_break = strpos( $stones, ' ', $key_end );
            $next_break = false === $next_break ? $end : $next_break;
            $next_key = substr( $stones, $next_start_at, $next_break - $next_start_at );
            echo "next_key = {$next_key}\n";

            if ( isset( $cache[ $next_key ] ) ) {
                $key       = $next_key;
                $key_end   = $next_break + 1;
                continue;
            }

            break;
        }

        // Nothing was cached.
        if ( '' === $key ) {
            $next_break    = strpos( $stones, ' ', $next_start_at );
            $next_break    = false === $next_break ? $end : $next_break;
            $key           = substr( $stones, $next_start_at, $next_break - $next_start_at );
            $next_stone    = rule( $key );
            echo "\e[31mno cache\e[m: rule is from {$next_key} to {$next_stone}\n";
            $cache[ $key ] = $next_stone;
            $next_stones  .= "{$next_stone} ";
            $next_start_at = $next_break + 1;
        } else {
            echo "\e[32mcached\e[m: from {$key} to {$cache[$key]}\n";
            $next_stones  .= "{$cache[ $key ]} ";
            $next_start_at = $next_break + 1;
        }

        if ( $next_start_at < $end ) {
            echo "{$next_stones}<<< @{$next_start_at}\n";
            goto find_next_key;
        }

        echo "\e[90mRound \e[33m{$i}\e[90m: \e[2;3;33m{$next_stones}\e[m\n";
        $stones = $next_stones;
    }

    var_dump( $cache );
}

p2();
