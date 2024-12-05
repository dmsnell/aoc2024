use std::simd::cmp::*;
use std::simd::num::SimdUint;
use std::simd::*;

pub fn p1<'a>(data: &'a Vec<u8>) -> usize {
    let mut lines: Vec<Vec<u8>> = Vec::with_capacity(150);
    let mut vec_at: usize = 0;
    let mut len: usize = 0;
    let mut count: usize = 0;
    let mut line_getter = data.split(|&c| c == b'\n');

    for _ in 0..3 {
        let line: Vec<u8> = line_getter.next().unwrap().to_vec();
        len = line.len();
        lines.push(line);
    }
    debug_assert!(vec_at == 0);
    debug_assert!(lines.len() == 3);

    // Process when four lines are simultaneously present.
    while let Some(line) = line_getter.next().map(|c| c.to_vec()) {
        if line.len() == 0 {
            break;
        }

        lines.push(line);

        debug_assert!(vec_at + 3 < lines.len());

        // Process while there are four columns.
        for i in 0..(len - 3) {
            debug_assert!(lines[vec_at + 0].len() - i >= 4);
            debug_assert!(lines[vec_at + 1].len() - i >= 4);
            debug_assert!(lines[vec_at + 2].len() - i >= 4);
            debug_assert!(lines[vec_at + 3].len() - i >= 4);

            let l1a = lines[vec_at + 0][i + 0];
            let l1b = lines[vec_at + 0][i + 1];
            let l1c = lines[vec_at + 0][i + 2];
            let l1d = lines[vec_at + 0][i + 3];
            let l2a = lines[vec_at + 1][i + 0];
            let l2b = lines[vec_at + 1][i + 1];
            let l2c = lines[vec_at + 1][i + 2];
            let l3a = lines[vec_at + 2][i + 0];
            let l3b = lines[vec_at + 2][i + 1];
            let l3c = lines[vec_at + 2][i + 2];
            let l4a = lines[vec_at + 3][i + 0];
            let l4d = lines[vec_at + 3][i + 3];

            count += is_xmas(&l1a, &l1b, &l1c, &l1d)
                + is_xmas(&l1a, &l2a, &l3a, &l4a)
                + is_xmas(&l1a, &l2b, &l3c, &l4d)
                + is_xmas(&l4a, &l3b, &l2c, &l1d);
        }

        // Process the remaining three columns.
        for i in (len - 3)..len {
            let l1a = lines[vec_at + 0].get(i).unwrap();
            let l2a = lines[vec_at + 1].get(i).unwrap();
            let l3a = lines[vec_at + 2].get(i).unwrap();
            let l4a = lines[vec_at + 3].get(i).unwrap();

            count += is_xmas(l1a, l2a, l3a, l4a);
        }

        // Skip removing the item to avoid the cost.
        vec_at += 1;
    }

    // Process the remaining three lines.
    debug_assert!(vec_at == lines.len() - 3);
    while vec_at < lines.len() {
        debug_assert!(vec_at < lines.len());

        // Process while there are four columns.
        for i in 0..(len - 3) {
            debug_assert!(i + 3 < lines[vec_at].len());

            let a = lines[vec_at][i + 0];
            let b = lines[vec_at][i + 1];
            let c = lines[vec_at][i + 2];
            let d = lines[vec_at][i + 3];
            count += is_xmas(&a, &b, &c, &d);
        }

        vec_at += 1
    }

    debug_assert!(vec_at == lines.len());

    count
}

const LANES: usize = 64;

// This was not faster than Vec<u8>.split(|&c| { c == b'\n' })
// My guess is that it’s because line-splitting isn’t in the hot path.
fn line_at_simd<'a>(buffer: &'a Vec<u8>, at: usize) -> Option<Vec<u8>> {
    let (prefix, middle, suffix) = buffer[at..].as_simd::<LANES>();

    for (i, &b) in prefix.iter().enumerate() {
        if b == b'\n' && i > 0 {
            return Some(buffer[at..(at + i)].to_vec());
        }
    }

    let newlines = Simd::<u8, LANES>::splat(b'\n');
    let indices = Simd::<u8, LANES>::load_or_default(&(0..LANES as u8).collect::<Vec<u8>>());
    let highs = Simd::<u8, LANES>::splat(0xFF);
    for (i, &sample) in middle.iter().enumerate() {
        let is_newline = (sample & newlines).simd_eq(newlines);
        if is_newline.any() {
            let offset = is_newline.select(indices, highs).reduce_min() as usize;
            return Some(buffer[at..(at + prefix.len() + i * LANES + offset)].to_vec());
        }
    }

    for (i, &b) in suffix.iter().enumerate() {
        if b == b'\n' {
            return Some(buffer[at..(buffer.len() - suffix.len() + i)].to_vec());
        }
    }

    None
}

fn is_xmas(a: &u8, b: &u8, c: &u8, d: &u8) -> usize {
    if (a == &b'X' && b == &b'M' && c == &b'A' && d == &b'S')
        || (a == &b'S' && b == &b'A' && c == &b'M' && d == &b'X')
    {
        1
    } else {
        0
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::assert_matches::assert_matches;

    #[test]
    fn p1_example() {
        assert_matches!(p1(&P1_EXAMPLE.as_bytes().to_vec()), 18)
    }

    #[test]
    fn p1_test() {
        assert_matches!(p1(&INPUT.as_bytes().to_vec()), 2685)
    }

    const P1_EXAMPLE: &str = "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
";

    pub const INPUT: &str = "ASAMXSMXMAXSAMMAMXAXXAXXMSAMXASAMXSASMSMXAXMAMMSSSMAMSMMSASMMSSMSMSMSXMMSXMXMAXXMAASMMMXMXMMXMAMXSSSMSMSMSMMMMAMMSMSXXAXMMXSAXMMXXAMASMXMASM
MMXXAMXAXSMXMMSSMMSSMSMMXSSMXMAASASAAAAMSMSMSSXAMXXAMXAAAAMXASXAAAAAMASASASMSSMMMSXSAMXAMAMSSMASAMAAAAAAXXAAXMAMXSAMAMSSMAMXMMSMMXSXAMAAASAS
SAMMAXMSMXMAXMAMAAMXAAASAMXSXASAMAMMMXMMAXMAAXMXMMXSSSMMSXXMXMASMSMSMXMASAMAAXAAAMMSMMXMXAXAASAMAMMMSMSMSSSMSSXSAMSMMMAAMXXXAAAAAMAMSSSXMXAX
AAMSAMMAMASXSMSMMMSMSMSMASAMMMMSMXMASXSSMSMMMSMSXAMXAASAMMMXSAMAAXAXMXMAMXMXXSXMMMAXASAAMSSSMMXSAMXAAXMAMXMAXAXMXAAXMMSSMMSMMSSSMAMMAAAMMMSM
MMMSAXSMXMXAMAXXAXXAAXXMXMASMAAXXAXMSAAXSAXAAAASMSMMAMMXSAXAXAXMMMMMMXMASXMSMMXSAMMSAMMXMXAMXMAAXXMSMSMSMMMSMMXSMAMMSAAAXAASXXAMASXMMSMAAAMA
XAAMAMXXSMMMMAMSSMMSMSXMXSXMMMXXMMSXMMMMSXSMSMXMAMMXAMMMMMMXMMMMMAXMAAMXSXAAAXAMAXMAMMMSSMSMAMASAMXAAAAAMXAAAXAXAASAXMSXMSSSMMMMAMAMXXXSMSSM
SMSSXSXAAMMAMXXAMAMXXXAMMSAMXMXSMMAXMASXMAMAAAAAXXXSMSXASXMSXSAMSMSMMMSAXMASMMXSXMASXAXAXAMSXSAXMSSMSMSMSMSSSMMMSXMASMAXAXMXMAAMSSSSMSAMAAMX
XAMXAMMSMMSASMMASMMXMSSMAXAMASAAAXMMSASAMAMSMSMSAMXSAAMSMAAAAXXXAXAAMXMASXXMMAMXMSXMSXSMMMMXXMMSAAXXAXXASAAAXMSMMASXMXAAMMXXMMXMAAMAAMAMMMSS
XSSMAMAXAASXXXSAMXMAXAAMSSSMAMXSMMSAMXSXSXXAAXAXMXAMMMSMSMMMSSSSMSSSMMMAMMAMSAMAMXAXSXSXMSAXXMAMMMMMMXMASXSMMXSAMAMSAMXSSSXMSSSMMSMMSMXMAAXA
AXAMSMSMMMSAXMMMMAMMMMSAMXMMXSAXXAMXSAXMXXSMMMMMXMSSMXAAAXXXMAAAAAMAMAMAMMAMMAMXSMMMMAXMASASXXAXXMXSSXMAMMMAMASMMSSMXSXMAMMXAAXMAXMXMXSXMMMS
SSSMXAXXXASMSMAXXAAAAAXAXAMXAMASMSMMXMSMXAMXMAMAAAMAMSMSMSSMMSSMMMMAMSSMSSSSSSMSMXSAMSMXMSAMXSMSSXAAAXMASXSAMASXMMAXASAMMMAMMSMMSMSXMAMXMAAA
MAAXMAMSMMSAAMAMXMMSMXSSMASAMMMMMMASMXAAMSMMMASMSMSAMXXAAMMXAXAAXXMAXXAAMAAXMASXAMSAMAAXXMMMXAAAXMSSSMXSSMSAMMSAMXAMXMAMXXMAAAAMAASXMASASMSM
MSMMMSMSAAMMMSMSASMMXXAXSASAMXMXMSAMAMMSMMASXXSXAAMAMXMMSMAMXSSMMSSXSSMMMMSMSXMMXXSAMXSMMSASXSMMSMXAAAXAXASMMASMMMSMXSSMSSSSSSMMMMMASMSASAXX
XMAMSAAXMMMAMAMXMXAAMMMMMMSAMXXAXMAXMSAAASAMXMMMMMMSXSAXAMASXAASAAXMAXXXXAMXSMXMMMMAMSXMASMSAAAXAMMMMSMMMXMAMAXAXXXAAAXAAAAMAMAXSSSMMXMAMXMA
XSAMXMSMXMSMSASXSMMMMAAAXMSAMXSSSMSMXMASXMAXSAAAMAAXAMXSXSAMMMMMMXSASMSMMMSASMAXAAMAMAXMXSAMXMMSMSMSAXAXSXMXMSSSMMAMMXMMMSMMAMXMAMXAXXMSMSMS
MMMMXSAXAAAASAMXSAMMXSMSMMSAMMXAMAAXXXXMASXSXMASMSSMXMAMXMAMAAAAXAXXAAAXAXMXMSAMXSSSSXMXMMXMASXAXMXMMSMMMXMAMXAAAAXXMXSXMXXMASXMXMSMMXSAAAAA
XAAXXMAMMSMXMSXASAMMXMAAAXSAMXMMMSAMXXXMXXXSXSAMXMAMSMSSMSAXSMSSMMSSMSMSSXMAXMMSXAAAAMAMSAMXAMXMAXAMXAMAXSSMSMSMMMSMMMAXMAMSASXSAMAXAXSMXMSM
SSSSSMSMXAXSAMMMSAMXAMMXSMSXMASXMXMSSMMSASMSXMASMSXMXAMAMMMMMAMAXMAXXAASMAMMSAASMMMMMAXAMASMSSSXSXMAMXSMSAAXSAXXXXXMASMSAAXMASASASMSSMMSAAAX
XAMAAAXASAXMAXXXSXMMMSAAXXMASASMMMMAAMXAASASAMXMASMMMMMXMASAMMSAMMAMSMSMMAMMMMMXAXXAXMSXSAMXAAAAAMXSXMMXAMXXSAMSMMXSXSXMAMSMAMXMASAAMAASXSSM
MAMSMMMAASXMMMMXMMXSSMMXSASXMAMMXXMSSMXMSMMMMXSAMXAMAMSMSASXSXMAMMMMAMAMSSSMMSASXMXMSXAAMMSMMMMMMSAXSXSXXXMMMMMXAAXMMMMMAXMMMSXXXMMMMMMSXMAM
SXMXSMMSMAXMAXSAMXAXAASMSAMXMMMSMSMMXMMMAMXMAMMAXMMMASAAMMSXMASXMXMXASMXAAAAAXMMSAASMXMXMASXSAASXMASMMSAMXXAAXXMXMMAAAASXMXAXMASMXAXXSAMASAM
AASAXMAMXAASMASASMMSSMMXMMMXXMAXAMMAXMAXAMAXMASAMXMSXSMSMAXASAMXMASXMMMMMSMMXMXAMSMXAAXAMXMAXMMMAMMMXASAMSSSSSMSMMASMMMSMMSMXMAAMMMAAMASAMAS
SSMMSMASMMXAXXSAMAAAMASAMASAMXMMMMMSSSSSSSSMSXSASAMSAMXAMXSXMASAMAXMAASAXXAASXMMMMXMSMSSSSMSMSMXAMAAMMSAMXAXAMAAAMMMXSMXAXXXXMSMMSSSMSAMXSXM
XMAMXMASXMAMMXMASMMMSASASAMAAAMMSAMAAXAXMAMASMMASMMMAMMAMAMMSXSAMMSMXMSSXSMMMAAXAXAAAXMMAMAAAAAXSSMSSXSMMMSMMMSSSMAXAAASMMSMSMAMXAAAXMASXMAS
MMAMAMAXAMSXSXXAXAMAMMSMMASMSMAAMAMMMMSMMAMMMAMMMMASAMSSMMSAMAMAMSAXXAXMMSAMSSMMMSMSMSMMAMSMSSMXMAXAMXMASAXAAXMAMMXMXMAMXAAAXSMMMMSMMSAXAXAS
SSMSXSXSSMAAAXMMSAMXSASASXMAAMXMSSMSAAAAMXSXSSMAASMXAMXMASMXSASAMMASMAMXASAMAAMXXXMAMAMMXMXXAMXASMMMSAMAMMSSMSMSMMMAASMSMSMSMSXSAXAXXMASMMAS
XAAAXXAAMAMMMMXXAXXMMASMMAXSXSXAAAASMMMSMSMXMXSSMMMSSMASXMAXMAMASMAMXMMMMMAMSSMMXSSMSMXSXSAMXXSASMAASMSMSAXAAMMMAAASMSAAAAMAAXAAMSASXSAAXMAS
SMMSSMSMMXSSXMMSMMXAMXMXSSMMAMMXMMMMSXAXMAXAXXMMMSMAAXASAMMMMSMMSMSMAMMXSXMXXXXMAMAMXXAAMXMAXAXXSXMXSAAAMMSMXMAMSMMMAMMMSMSMSMSMMMAMAMSSXMAS
XMAMAXMASAAAAMXAAAXSSSMXSXXMAMMMXMSAXMMMSASMSMXAAAMXSMXSAMXAAXAXXAXSASMASAMXMAMMASMMAMMSMAMSMSMXMASAMMMSMXXAXMMXAMAMXMXMAXAMXAMXMXAMXMMMMMAS
MMSSMMXMMMSSMMSMMMSMAMMMMAMSMMAXSAMXSASAXAXXAXMMMMMAMXAXAMSMMSMMMMMSXSMASAMAMAMMAMAMXMMAXAXAAAAXMAMASXAXMXMMMSXAXSMSASASXSMSMXMASXXSXXXAXMXS
MMMAXMAXMAMXMXAXAMMMMAAAMAMAMXMSMXXXMXASMMMXAMXSASAMXMMSMMXXMAXAASMMMSMMXAXSSMSMMSMMSXSASXMMMMMAAMSMMMMSMXAAAXMMXAASXSASAXXSAMXAMXSMMMSMXAMX
SAXXMSSSMMSASXXMMXXXXXXXXASASAMMMSXMXXMMSMAMMAXMASAXXSMSAAMXAMMSMSAAAXMASMMAAAXXAAMMSAMXXMASXMSSSMAXAXMAMSSMMSAMMMXSAMMMMSMXAMSAMSAXAAAMMSMM
SAMSAMAAAASXMAMMSMMSSSXSMMMAXXSAAXAMMMXMAMSASMMMMMXAAMAMMASXAXAMMXMMMXXMASMSMMMSSMSAMSMXMMAMAAAAAAXMMMSAXAMXAXAXMXAMAMXAAAAMAMSAMAMMSMMMAAAX
MAMXAMSMMMSASXMAAAXAAXMSASMSMAMMSSSMAAAMXXXMSAMXSAMXXMAMSAMXMMMSASXSSMSMMXMMAMAMMXMASAMAXMASMMMSMMSMSAXMSMMMSMXMMMXMMMSMSSSMXXMAMMXMXASMSSSM
MAMXAMXAXXSAMAMXSSMMSMASAMAAAXMXMAMXSSMSSSMXSXMSMAMMMSXXMMSAMXMMASAAMMSAXMSSSMAXXAMXSASAXXAMAMXMAASXMASXAAXSXAMMMMSMSASAMXXXXXMAMXAXMAMAXAAA
SASXAXSXMXMASXMAMMAMAAMMXMXMSAMXMAMAMAMAAAMASASXSAAAAAXSAAMMSAAMAMMMMAMAMXAAASXSSXSMSMMMSMAMXMAXMMSAMMAMSSMSMSMAAMAAMASXSXMAMSMMXSMSMSMXMSMM
SAMMSMMAAXSAMMSXXMAMMSMMAXASXXSASAMMSAMMSMMASMMAMSMMXSXMMMAAXMMMSSSMMSSMSMMMMMMAAMAAXXAASXSMSSXSAMSXMXMAXMMMAAMXMSMSMXMAMMMAAAASMSXSAAAAXXXS
SASAMASXSXMAMAAASMMXAAASMXXMAASXSXSASAMXXXMASAMAMMXSMMMXSMMMXSXXMAXMAXAAXMSMSAAMMAMSMSMMSAXAMAXAAMMSMXXXMSAMSMSMXXMAMMMAMXMMSSSMAMAMSMSMMSAX
SAMASAMAMMSSMMMSMAAMSSMAMSMMMMXMSAMXSASMSAMAXAXSMSASAAXAXAXMAXMMMAMMSMMMMXMASXXXXXXAAAMMMXMSMMMSSMAAXSASASMMAMAXMAMXMSSSMXMAAXAMAMMMAMAAXMAM
MAMMMMMSMAAMAASAMMMMAAXMAAAXAMXSMXMASXMAXXMAXSMXAMAXSSMSSMMMMXAAMMSAMAMMMMMAMMSMMMSMSMSXAMXXAMAAAMSSXSAMASXSMSAXMAMSMAAAASMMSSSSSMXMASXMAMMA
SMMAAXXAAMSXSMSASMSSSXMXSXSMSSXMASMASAMSMSMSAAMMSMSMMMAXMASAMSSMSMMASMMAMAMASXAAAAXAMAMXMSASMMMSAMXMAMXMXMAMAMAMMAMAMMSMMMAAAAXAAXSSMSAMASAM
XAXMMMMXXMXAXMMMMMAAMAMXMAAXXMASXMMSSXMAAAAAMMXAAAMAAMMMSAMAXAMMMMMXMXSAXAXMMXXSMXSAMXSAAMAAXXXMASMMXMMMMMAMAMMASMSMSAXAMSMMMSMMMMMAMSAMASAM
SSMSSSSSSXXMXAAXAMMSMMXAMXMXXMAMAXMASAXMSMSMXXMSMSSSMSAAMMSXMASAXAMMXASMSMSAAMXMXMMAXXMMXMXMMSSMASAMMAMXASMSXSMMMXAXMASXMASMXXAAMAMAMXMMASAM
AAAXAXAAMAMXMMMSSMMAMMMXMAXAMMMMMMMAXXMXAMXXXMAXAMAAASXSXMAXSXMMSMSAMXSXAASMMMASASMXMXMASAMXAAAMXSAMSSMMXSAMASXXASAMSXSAMXSMMMSASXMAXAXMAXXM
MMMMMMMMSASAXAMXXAXXSAMMSSSMAAAASMSMSSXMASMMMAAMAXXMXMAXAXAMXMMMSXAMAMXMMMXMAMXSAMXMMAMASXSMMXSMAMAMAMAMASAMXMAMXAMXMASMMASASAMXAAASMSSSSMSS
MSMSAXXXSASASXSMSSMAXASAMMAMSMSMSAAAAAASASMMXMASMMMXMXAMXSXMASAAXXMXSAAAXXASXXAMAMAAXAMAMAMXXAXMMSSMXSAMXSAMXXSMAAXAMAMXMASAXXXMMMMXAAMAAAAA
XAAXASXMMAMAMAAMAMASXMMXXXAMXAXAMMMMMSXMAXASAMAAAXSMMMSSXMASXSMXSAMAMSSSSSMMSMXSXSXMSXSXMSXMMMSAXXAMASASAMAXAAMSXMXMMSAMXAMMMSXXAAAMMMMSMMMS
MMMMAMAMMSMSMSSMXSAMXSMSMSMSMMMMMSMMXXAXAMSMSMSSSMSASAMAMSAMXXAAMAMMXAXXAAMAMMMMAAMASAMXXMASAXXXMSMMASMMASMMMXAAXAMSAMASMSSXAXMAXMSSMXMXXAMX
XAASASXMAXMXXXMAMMMSXMAAAAMAXAAAAAAMSSMMMMXAMXAMXASXMASAXMXSSMMSSMMSMMXXSMMAMAAMSMMASAMSASAMMMXAXXXMASASAMXXXMMXSMMAASAMAAXMSXMSAAMMMAMMMSSM
MSXSASAMXSMSSMMAMMASMMMMAMMAMXSMSSSMAXMAXAMAMMMSMXMMSAMXSSMMAXAMAMAAASMAMXSXSSSXMAMXXAMSAMASASXXMAXSAMXMAMXMXMMAMAASMMMMMMXMAMAMMMXAMXSAAXAX
XMMMAMXMAAXMASXSSMAMAAAXSMMSSMMMMMAMAXMXMMSAMMMXXAAXMXSMXMASAMXMAMXSAAXAMMSXAAXAXXMAXSMMAMAMASAAMSMMMSASMMSSSMMASXMMXXXXAMXMSAMXXSSSMMMMMXAM
MSAMXMAMSMMXAMXMAMXSSMMMXAMAAAMXXSAMSMSAAXSASXMAXMSMMMMXAMXMMSSMASAMMMMASASMMMMSMAMMMXASAMXSXMMMMAAAASXSAAAAAXSASMSMMMSSMSXAXSXMXAAAXMMAXMXM
ASAMSMXXAASMSSMSAMXXMASMXAMSSMMAMAMXXASMSMSAMXMSXMAAXMAXMXMAXAXXMMXSAXSAMXSAMAAAXAMXASAMXMAXAXXSXSMMXSXSMMMSMMMXSAAAAAXAAMMMMMMMMMSMMSMASMSM
AMAMMAMSSSMAMAASMMMMXMASMSMXMAMSSMMAMXMXMAXMXMAXAMSXMMSSMASXMXSAXAXXXXMXXASAMMSSSMXXAMAXAMXMMMMSAXXSAMMXXAMMMMMMMXSSMSSMMMAAAAAAXXAXAAMMSAAA
MMMMSAMXMXMAMXXXMASXASAMXXMMXSMMAXMXSASAMMMMXMAMAMMXMAXAMASAMASMMSSMSSSSMASXMAXMAMSMSSMXMSMMSAMMAMAMAMAMSAMAAAAAMXMAXMAMXSSSMSSMXSASMMSAMXMM
SASMSASXSAXXSXMASXSXMMXXXAXSAMASMMXAXAMMXXAXXMSSSMXAXASMMMMAMASXAMAASAASMXMAMXSMAMXAAXMASAMAXASMXMMSMMSXMASMSSSSXSXSMMMMMMAXAXAMAMXAAMMMSMMM
SMSAMASASMSMMMSAMXMASMSMMMAMASXMMAMSMSMSMXSSXSAAMASXMAAXAMSSMASMMSMMMMMMMMMXMASMSSMSMMSXXAMASXMXAXXXASMAMAMMAMXMASAMXXAAMAAAMSAMXSMXSMAXAAAX
XXMAMXMAMSAAAAMASASMMAAAASXSXMMAMAXXAAAAAAXAMAMXMXXMASMSSMAMMMXAMMXXAMXAAASAMXSAAAAMAAAMSXMASAASMXXSMMMAMASMMMXMAMAMMXSSMMXSXAXSAXXAMMSSMSMS
MSSMMSMMMXXXMSSMSASAMSMXMXAMXXAXSMSMSMSMSMXASXXAXMAXMAXXAMXSXASMMMMMMXSSSSSSSXMMMXXSMMSAAAMXMMMMXSMXMASASXSASAXMMSSMAMMMMSAMXSSXXXMASAXMAMAS
AAAMAMASMSSMMAMAMAMXMXMXSMSMSMMMXMAAXXXAAXSXMMSXSXXMSMMSXMMXMXXMASAASMAAMAMAMMMXSXMAXMAMSSMSXAXSAAAXMASASASAMMXAAAMMXSXAAMASAMAMMXMSMXMMSMAM
SMMMXSMMMAAAXAMMMMMMMAMAXAXAXAMSASMSMSSSMMXAAXSMMMSMAAXMASASMSMSAMSSMAMAMAMAMSMAMXAXXXMAXAAMXAMMXMMXMMMXMXMXMASMMMSAASXMMSSMAMMAXAXXXAMXXXXA
AMAMXAMAMSSMMXMAAAAAXAMMMSMMMSMSAMAXAAXAXXSSMMMAAAAXMSASAMMAAAMMSMXXMSXASASAMAMASASMMMXSSMMASXXMMSMXMXMXAMXXXXAXSAMMXXAXAXXSXMASXMSMSMMMMMMA
MSASXMSAMMAXAMSSSSSSSXSXAAAMXXXMAMAMMSMXXXAAMSSSMSSSSXMMMSSMMMSAMXAXMASXSAMMSXSAXXXAMXAAXAMAMAAXAAAAMASMASXSMMSXMASMSSSMMSXMXSMAAAAAAMAAAAAS
XSASXXSXMSAMXXAMAAAAMMSMMSSMMMMSAMXXAAAASMSAMXAXMXMXMAXMMAAXXXMMMMMMSAMXMMMAAAMMSMSSMMSMSXMASXMMSSSXSAXSAMXMAMMMSAMXAAMAXSAMASXAMSMSMSSXSSSM
XMASMXMAMMASXMASMMMXMAXAXAAAAXAXXXSXSMSMXAAMXMMMSAMASXMSMSSSSSMXAMXAXAXXAAMSMXMAXMAMAXAASMSXSAMAXMAMMXXMXSXXAMAAMASMMMMSMXAMASAMXAAMMAMAAMAM
XMAXMAMSMSXMASXMXXAAMMSMMMSSMASMAMSMXXMAMMMSSXSASASAMAAAAAAAAAAASMMMSMSMSMXMXXAMXMSSSMMSMAMMMAMSMMSMSMMMMMMASMMXMMMMXSXXMSXMAXXSSMXMMASMMMAM
AMASXMXAAAAXMAMSAMXMXXAXAMAAMAXXMMSAMSMMSAAXMAMASMMASMSMMMMMMMMMMAAAXXAAAXXMASMXMMMAMXXAMAMMSMMAMSAAAAAAAAAMXASMSMAAAMASAAAMMXAMXMASMMSAXMAS
XMASASXMSMSSSMXSXMASMSMSSMXXMXSAMAXMAMXAMMSSMXMAMASXMXXXXXXMAASXSSMMSSMSMSSMAXMAMAMAMMSMSXMXASXXSXMSMSMSSSSXSAMAASMMXSAMXMMXXAMXXMASAMMMXSAM
XMMSMMAAAXXAAMXMASMSAAXAXXAMSMSXMAXSSMMMSAXAMMMSMMMMSMMMSMMSSSSMAAAAMAAMAMMMSSXSSXSXSAAMAMXMASXXMAMMAXAAAMXMAMMXMXXAXMMMMXMAMSXMSMAXASAMXMSS
SMXSXSMMMMMSMMASXMAMMMSSMMXSAAXMASMMAAAAMASAMMAMAXXMAAAAXAAAMXMAXSMSSMMMAMAMXAAXAMXMMMMMSASMAMASXSMMAMMMXMMMMMSMXMMMSMAAXAMAMAAXXMAMXXAXXAAS
SXAMMXAASXAXXSAXAMXMAMAXAMXXMXMMSMASMMMSSXSAXMASMMSXMMMXSMMSSSSSMMMAXMAMASASMMMMAMMXMASXMAMXAMXMXAMMSSXMXMASAAAAAAAMXXSMSMSXSSSMMMMMSSSMMXMS
MMMMAMSMMMXSAMXSMMMXXMASXMMMSMAXMXAMXXMAMAMXMMAXAAMMSSSXXASMXAAMAXSMXXMXMSAXAXXXMSMXSASAMAMSXSXAXASAAMAAXXAMMSMSMSSMAMMASMSXAAAMSAAAXAMXAAAX
SXMASMXASAAMAMMMAAXMSXMAXAAAAXMMSMSXSXMASMSXSMSSMMXAAAMAMSMMMMMMMMMXSASAMMMMSSMAMAAXMXXASXMMAXMAMXMMSXSMMMMSXMAXXMAMXSMAMASMMMMMSMSSMAMXSMMM
AXSAXASAXMXMSMASMXMAXAXAXXMSSXSAXAAASMMMSXMMMAXMASMMMMMSMXAMXMXSAMSASAXMMAMXMAXSMMSMMAXMMMAMAMMSSXSSXXMAXAAMASAMASAMXMMXMAMAXXXMXAAXXXSAXAAA
MMMAMXMXMMXXAMASXAASXMMMSMMXMASXMMMXMAAXMAXAMAMMAMXXAMXAASAMAMMSASMAMMMSSMSSSMMXASMMXMASAMAMMSAAAAXASXMSMSMSAMXSAMAAMSSMMXSMMMMMMMMSMXMASMMS
XXMAMASXSAASXMASMXXMAMAAAMMSMMMXSXMXSSMSASXSSSMSASMMSXSMAMMSAXXMXMMXMAAAAAAAAXASMMAXMXAXASXXAMMSMSMMMXAAAAAMXMAMAMAMXAAXSAMXXAAXXSMXMAMXMAMM
SMSMSMSAMXMSAMXSAMMSMXXMXSAAAAXMASXAXAXMXXXXAAXAASAAMAMXMAMSASAMSXXSSMSSMMMSMMMSXMAMMMMMXAXMASAXXMAAAMSMMMSMSMXSSMMXMSSMMASMMSXSAMXMSXMASAMS
AXAMXAMAMXAMAMXMMSAAMAMSMMMSSMMAMXMXSSMSAMMMSMSMMMMAMAMSMMXSAAMAAASAMXAMAMXAMXASAMAAMAMXMAAMXMASMSSMMXMAXAXAXXMAMAMXMAXASAMXAAXMAMAAAXMXXXSA
MXMSMMMAMXMXSMSAAMXMMASAAMAXAMXMXAAXMAAMXXAAAXAXXAXAMAMAAMASAMMXXXAXXMXMMMMXAMXMAMASXXSXMXMASMMSAAXXMASMMSMSMSAXMSMMMASMMASMSSMXAMXMASMAXXXM
XAXAMXXAXAXXXAMMMSSMXXSXSSXSAMSSSMSXSMMMXXXSAAMMSASMSSSMSMAXAXMSSSMXMSSMAASMSMSSMMMMAXSXSAXXAAAMAMSAMXMXAMXXASMSMXAAXXSAMXMXAAXSSSMXAAMMSMAA
SMSASMSMSASMMXMAMSXSAAMAMAAMAMAMAAXMASMSSXMAMAXAXAAAAMMMXMMXSMSAAAXAMAMSMMXAAAAASMMMMMXASMSMSMMMXAMXMAMXSXMMXMASAMSMSMXMMSSMMMMXMAMMSXXAAMMM
AASAMAAXXXMMAXSSXMASMSMSMSMSAMMSMMMAMAMAMMMSSSSMSAMMMSMMMASAAXMMXMXXMAXSMSMSMMMSMSAAMMMMMAXXAMASXXXAMASXMAXSXMSMMMXAMXXAAAAMASXMSAMXMMMSSMSX
MAMAMMMSMAMXMMXMAMAMXAAXMAXMAMXXXAAAMAMXMAAMAAAAXAXMAMAASAMSSMSXSMSMSMSMAXAXSSXMASXMAAMAMXMMASASAMSMSASASMMMAMXAXAMAMASMMSAMXSAAMMMAXMAMAASX
XASAMXAXXXMAMXASXMSSXMSMMMMXSMMSSMSXSMSMXMXSMSMMSMMMASMMMAMXXAXAXAAAAMAMXMXMASAMAMMMSASASAMMAMXSAMXMAXSMMMAMMMSMMMSAMXSXAXAMXSMMMMSASMSXMXMA
SASXSMXXMXSMSMMMMMAAXSAMXXAAMAMXAAMAXMAXAAMSAMXXAXAMAMAASXMMMMMMMSMSMSSSMAAMXMXMXMAAAXSASASAMXASAMASXMMXMXAMAAXXAXXAMMXMMSMMMMMMAMAAMAMASMMM
XAMXMXSAMAMAMAAAAMMSMSASMMMMSAMSMMMAMSASMSMSASMSMMMSSSSMMXMAMAAXMMMMAAAMAXXXSMMMMMMSXMMAMAMXXMASAMXMASAAMMSMMMSMSSSXMMXSAAAASAAXAMMSMXMASAAX
MSMSAASAMMSASXSSXSXMAXAMXAXASAMMASXXAMASXMAMAMAXSAMXAAAMXXSASMMSASXMMMSMSMSXAAMXAMXMAAMXMAMXMMAMMMASXMSXSAMAMMAMMMMAAXAMSSSMSSMSASXAMAMASMMS
XAAMMXMXMXMAXAAMMSMMMMSMMSMMSAMSAMMMAMXMASXMMMSMSSSMMMMSSXMASAASAMASXAXAAAASMMMSMSAXMMMSSXSAMMSSMSXSAMAXMAMAMSMSAAMXMMXXAMAAXAXMXMXSMMMAMAXM
SMXMXMSMMXMSMMMSAXXAAMAAAXAXSXMMXSASXMMSAMASXAAAMXMAMAXAXAMSMMMMAMXMMMSSMSMXMAMAMMMSMAAXMASASAMAAMMSXMASMXXAMAMSMXSASAXXMSXMMSMSXMAXAMMMSMMM
AMASAMAMXMAMXAAMASMSSSSSMMAMMMMMMMXSAAAMASAMMXMXMAXAMXMMSXMASXXXAMXSXXAMXXAASMMASAMSMMSSMAMMMMSSMSXSMSAMAASXMMMXAASAMSMMXMXMAAAAAMASMMAMAAAM
MMSSXSASXMSMSXSMAMMMAAAAXMMAMXAAAMASMMMSAMASAXSASXSMSMAXMASASMSSSMXSMMSSSMSXMXXXMAXXXXMMMSSXAXMASAMMASMMSMSAAXMMSMMXMAMMAMMMSSSSSMXMMSXSXSSS
XSAMXMXSXAAASXMMMSMSSMSMMXSAMSSXSMASASAMXSAMXAXMMXSAAMAMSAMXXAAMAMAXXAAMAAMXMSMSMSSMMMSAAAXMASMXMMAMAMAXMAXMMMMMMAXMSSMSAXSAAAXAXXAAMMMMMMMM
MAAMSMMMMSMXMAXXAAAXAAAXAXXAMAMAXMMSAMXSAMXSSMSXSAMSMMSAXXMMXSXSAMSSXSXSMMMAXAAAAAMAAASMSXXXMXXSMXSMMSMMMSMMXAASMXMASAASASMMSSMMMSSSMAAAAAAM
SSXMAAXMAXXASMMMSMSSMMMXSMSSMAXMMSXMASAMXSMMAXAXMAMMXMXMSMMSAAMSASAMXMMXXMXXXMSMSMSMMMSXMMSSMSAMAAMAXAAMAAAXMSXMASAMXMMMXMAXAMMXAMMAXSSSSSSS
AASXSSMMMSSMMAAAXXMAMXXXMMAMSMAAMXAMAMASMXASMMXMSSMMXMAXXAAXMMMMMMXMASASMMMSAAXXMASMMXMAMAAASMAMXSSMSSSMSSSMXMMSSMSAMXMXMMXMSSMSMMSAMMAMAAXX
MMMXAMASMMAMSSMSMXSAMXXXAMAXAXMMSXSMASAXMAXAMSSMMMASXSMSSMMSMSMSXSASASMMAAAXXMAXSASAXAMAMMMSAMAMXMAXAAMMXAMMMAAMAAXXAASAXXXAXAXXAAMXSMMMXMAM
MSMMXXAMMXAMMMAAAASAMMXSMSXSAXSASAXSMMMSXXSXMASAASMMASAAXMAAXAAAASAMXSMSMMXSAMXMMAMXXMMSXXXMMMMXXMAMMXMXMXMAXMMSMMMSSXSASMMMSMMMMMSASAMMAMXM
MAAMSMSSXSASAMXMMMSAMSMMAAXMASMAMMMXSAXMAMMXMASMMSAMXMMMMMSSSMSMMMXXMMXAAAASMMMMMMMSXMAXMAXXXASXSAXSMXAXSXSASXMSAMAMAAMMMAAAAAAAAMMMSAMSMMAM
SMSMAAAAASXMASAXXAMAXMAMMMMXXAMXMSMAMMXAMXAMMXMAXSMMSMAMSMAMAXXXXAXSXMSSSMXMMAXMAMAXAMASAMMXSXSASAMAXAMXMXMAMAAXSMASMAMASMMMSMMXMXAAXMMAMSAS
MAAXMMMSMMXSMSASMSMMMSSMSSMSXSAMXAMASMSMSXSAAMMSMMMAXSASAMASAMAMMSMMAMAAXMMMSMXSASMSSMASMMMXXXMMMMSXSXMAMMXMSMMMMMMXXMAMMAMAMAAASMXMXAXAXSAM
MSMSXAXXAMXSAMXXXXASXXXAMAASXMAXSASASAAAXAAXAAAAAXMSMMXSASXMMMSXAAASAMMSMAAXAMMSAMMAAMASAASXXMAXAXSXMASMSMAMAXSXMASXMASMXSMAMMMMSAAMSSMSMMAM
XAXMXMSSMMMMMMMMMSXMAMMXMMMMASAMMASMMXMMMMMMSMSSSMMXAMASMMMXSAXMXMMAMMAMXSMSASAMSSMMMMXSXMXXMASXMSMMSXAAAMSSXSAXSAMAMSMSAMMMSMMXXMMMAAXXASAM
XMSMAMAMXAAAAAAAMMMSAMAXSAMXMMMMSAMXAAMSSXSXXAAMAAASAMXSAXXAMASMMSXMSSSMXMASAMXSASASMMXMMSSMSAMXMAMAXMMSMMMAMMMMMASXMAAMXXAAAASMMMSMSXMMAMXM
MSAMAXASXSMSSSSXXAASXXXSAMXSAMXAMASMSMSAMXXMSMMXSMMMAMASXMMXMAMAXMAXAAAXMMMMAMXMASXMAMXMAMAXMXMAXAMMMAXAMAMAMMXMSAMASMSMSSMMSXMAMAAMXASMSSMM
XMAXMSMSAMXXAMXMMSXMXAMXAMAMAMMMXAMXMMMMSMSMXAAXXAMSSMMMAXXAMXSMMMMMMSMMXAAXAMXAMXMSAMAMSSMMMMSXSASXSSSMXAXASXAAAXSXMAAAAXMXXXSXMSSSXMMAAAAM
MXSXAAAXMXMSAMAMMXAMSSSMMMXXAMXSMSSMXAAXAAAMMMMXSAMAMASXMMSMSAMXSAAAXAXMSSSSXSXXMAXMAXAXMAXMAAAMXAAMXAAASXSMSMSSXXMAMXMMMSMXMASXAMAMAXMMMSSM
SAMMSMSMMAMXAMXMAMAMAMAAMSXSXSAAAAAASXSSSSXMAASXSMMXSAMASXMAMXSAMMXMMMXMAAAAASMASXSSMXSMSAMXMSSMMMAAMSMMMAAAXAAMMMSSMSXSMAMXMAMMAMASMMSAAMAA
MASAAAAMMASXSMSMXSAMASXMMSAAASAMXMMMAAMAAAASXMMMSAMAMMSMMAMSMAMMSSMMMXSAMMMMXMAXAAAMAAXXMXSXAAAXMAAMMAMAXSMMMSMMAAAXAAAMMAMXMASAXMASXAMMSSXM
MAMXMSMXXASAXAAMMMASASAXAMAMMMXXMMXMMMMSMMMMSAAASAMXMAAAMAMXMAXMAAAAAAAMAXXXAMSMMSMMMMSAMASMMSXMMSMXMASMMMXXAAMXSSMMSMAMSMSMSASMXMAMMMSMAMMM
SSSSXMXMASMXMSMSASAMASMMAMXMASMAMXAMXXAAXXXAXSMMSXMASXSSSSXXMAXMXMMMMMSXAXXSXSAAXAAAXMMAMAXAXXMMMAMASAMMAXSMSXSAAAXAMMXMAMAAMAXMXMASXMAMAMAX
SAAAAMSAMXAXMXXSXMMSMMMSMSMMASASMSSSMMMSSXMAMMSMXMXMAXAXAMXSMMSMSASXAXAMXMXAXSXXMSSMSSSMMSSSMMAXSASAMASXSMMXMAMMSMMXMSASASMSMASXMSASMSSSSSSS
MMMMMMXMAXMXAAMMMMAAXAAAASASAMXAAXAAXASAMXMMMMASXASMSMSMSMAMAMAASAMXXAMXAAXMMMXXMAMXAAXSAMXMASMMMXMAMAMXAAXAXMMAAXXAXXASXSAXMASMAMXSAMAAAAAA
MMXMASXSSMSMMMSAAMSMSMMSASAMASMMXMSMMSMASMMSASAMMMXAMAMAMMXSAMMXMAMASXXSXSMXAASMMAXMXMSMMMAXXMXAXMSSMAXASMMMSXXSASMMSMMMXMXMMAMMAMMSMMMMMMMM
MASAMSAAASMAAAMMSXXASXAXMMXMMAMXSAAMMMXAMAAMMMAXAXMXMAMAMAMSASXXSAMAMSAMXMASMMMASAMSAMAAMSSSMMXSAAAMSMMMXSASXMAXMMMAAAASXSXAAAXSASAMXXSASAXX
SASMXMMMMASXMSSMMAMAMXXXAMXSMAMAMMXSASMMMMSXMXSMSSSMSXMAMAMSAMXAXXMASXMASMXMSASMMMXSASMMMXMAMAAAMMMMAMXMASXMXMAMAAMMSSMSAMMAMXMXAMXSAASASMSA
MASXXMXAXMMXAAAMXAMMMMSSXMASMXMASMMMMSAXSAMXSAAMAMAAXMSMSMXMMMMMAMMMSMMASXAXSMMAAXAXAMXSAXSAMMMSXMMSMSXMAMASXXXXMMSAAAAMMMMASMSMSMSMXXMAMAXA
MMMMMMMMMXSMMSSMXXXAXAAAAMAXMAMXMAAAASXMMASAMSMMASMMMMAMAMAAAAAAAAAAMAMASMMMMXSSMMMSMSMAAMMMSMAMASXAXAAMXSAMMSMSMAMMSMMMAASXMASAMXXMMMMMMMMM
SMAXAAAXXAXAMAXASXSMSSMMXMASXXSAMSXMMMXXMAMAMAMSMMXAAMASXSSXSMSXMMMMSXMAMXMAXAAMAXXAAAMMSMXAAMMSAMXMMSXMXMMAMXAAMXMAMXXMSASAMSMMMSSMAMASAMAS
MXMXSXSXMXSXMASAMAAAAXXXAXAXAAXMMAXSAMMMMASMMMMAAMSXXXASAXMAMAMXSASXMAMMMSXMSSSSSMSMSMSXAXMXSSXMAMASAMMSMXSMSASMSXMXSMSXXAMMMAAMAAAXMSASASAS
SASAMMXASASAAXMAMSMMMSSSXSMMMMMSMSMXAAAXMASAAMSMSAMASMMMMMMXSAMASASMSAMXASAMXAAAMAXAXXAXXMMSMXXSXMMMASAAAAMASAMASMSMAAMAMXMASXSMMXSMAMASAMAS
SXMASASAMASAMXSXMMAXSAAXMXMASAXAAXASXSSXMMMXMMXMMAMAMAAAAAMAMAMXMAMAXSXMASAMMMMMMSMMMSMSSMSAAXMASXXMAMXMSXMXSAMXMAAXMMMAMASXSXMASXMAMMMMAMSM
XMASMAMAMXMAXASAMSSMMMSMSASXSMSSSMAMAAAMSSSSMXXXSXMAMMSMSXSMSMMMMMMSMMXSASAMAAXAAXAMASXAAXAMSMXXAMXMASXMAXMASMMXMSMSMMSASXXSXASMXASASXSMMMAM
SAMXMMMMMMSSMASAMAAXSXXXSXXAXMAMAMMMMMMMXAAAXXSAMXSSSMMXMXSXMASXMAAAAXMAASAMSXSMMXSMASMSSMXSAAMXMXMMAMXMAAMAMAAXXAXAAASAXXMAXAXMSMSAMXSAASXS
AXSXMAAAAXAXMASMMSAMXAXAMMMSMMSMSMSASMMSMMMMAMMSMXXMAXXASAMAXAMASMMSSMXMASXXMAMASAXMXSAAMAXMMSMSMASMMXXMSXMSSSMXSSSMSMMMAMXMMMAAXXMAMASXMMAM
MMMASMSSSMSSMASXXXXMAXMAMAAAAAAAMASASXASAMAMASAMXXASAMSMMASAMXSMMSXAXAMXMSMSXMSAMXSSSMXMXSAMXMAMMAMAAASXXXXMXAXMAAAAMXXAAMAXAXMSMMSMMASASMAM
XASXMXAMXAMXMSMMMMAMAMSXMMSSMMMSMAMMMMMSASXSMXAMMMMMAMAMMMMMSXAXAMMMMSXXXMAMAMMMMAXMAMAXMMMASMMMSMSXMMSAMXSMSMMSMSMMSSSSXSXSSXAAMXASMXSAMSSS
SASXXMSMMSMXMAAAXMAMAMMASMXMMSAAMAMXXAXXASAXXSMMASXSXSMSXMAAXMAMXMAAAXMMXMXMAMAAMMMXAMSMAAXMMAMAAXMASXMAMAMMMXAMXMAMXAAAAXMAXMSMMSMSXAMXMAMM
SAMXAAAAAXMASXSMSSXSXMSAMXSAASMMMAMMMSMMMMMMXAXSAMAMXSXMASXSMSMSASMSMSAMAMASMSXMAASMMMAASMSXSAMMMXXASASXMASASMMSAMSSMMMMXMASXMAMMMMXMASAMMSM
XAMMMSSMMMMAMMAAXAASAMMSSMMMMXAXSXSAAAXAAAAXXMXMASMMAMAMMMXAAAAMAAAMXSXSASXSXMAXMMMASXMMAMXMSMSMXAMXSAMXSASASAAMAMAAASXXAASMMMAXAAMAMXSXSAMX
SAMXAMAMSSMMSXMAMMMMAMAMMXAXMSSMMSSMSSSSSSSSMXMXAAAMASAMXXSMMMSMSMAMXMASASMXAXAASXSMMAMMAXMMSXAAMSMMMXMAMMSMMMMSMMSSMMSSXMXAMSXSSSMAXASAMXSX
MMXMASXMAAAAAAMAMXASMMXSASMSMAXAXMXXXMAMAXAXMASMMSXXAXAXAAAAAXXAXAMXAMAMMMMSMMSXMASASAMSMMMASMMXMAAXMAMXSASAXXAAAMAMXMXMSXMAXAXMAMXXMASXSAAM
XXXSXMXMSSMMSMSASXXXXXSMMMMMMASXMMMMXMAMXMMMSXSXXMMMSSSMAXXSMSMMMASMMSXSXAAAAAMAMAMMMAMASMMASXSASMSMSAMAMAMMMMSSSMMXAAAXAXSXMSMSAMXSSMMMMAXA
MSMMAAAXMXAAAASMMMSMXMASAAASAASAASAAMSASMASAMAMXAAMMMAMMMSAAXSAAMXMAMMAMMMXXMMSAMASXSXMMMMMMXASXSAXXSASXMSMSAXAXAAMSSSSSMXSXXXAMXMXXMASASMSS
AXASMMMMMSMMMXMMSAXMASMSSSSSMXSMSSMXXAAMXAMASAMSSXMAMSMAAMMMMSMMSSSMMSAMASAXSXMXXAMXSAMXASXAMMMMMAMAMMXAAAASMSMSMMMAAMAAXASMSMAMMMSASAMXAAAX
MSXMXMXAXAAAXMAMASXMAMXMAXAXAXMMAXXSAMMMMMSASMXAMASXXAMMMSASMXASAAAAASAXAXMXMAXASXXMXMXMMMXAXMAMMAMAMASMMMXMAAXAXXMMMMSMMMSAAMAMXXAAMMSAMMMM
MXMAASASMSSMXXMMAMMSSMAMXMMSXMAMXSMMAXAMAAMMXXMAMMMSSMSXASXSAMXMASMMMSAMSSSMSMMMXAASXMSSSMSSSSMMSXSXSMSSMSSMSMSSSMASXAXSMASXMSSSSMMSMXXMXMMM
AXASXMAXXXXMASMMMSAAASMMMAMAXMXMAXXSAMMSMSSMXSSSMSAAMXMMMMXXMSMSXMASAMAXAAAAAXAXMSMMAMAXAAMAAMMAMASAMXSAMAAMAMXAAXAMMSMAXAXSXXAAXXXAASMMASAM
MAXXSMMMMSMMASXAAMMSXMSAAXMAXMAMSSMMASXAXXAMAAAAMMMMSAMASXMSAXAAAAXMXSMMMSMMMSASMMMSAMASMMMMMMMAMXMSAMXMMSMMASMSMMMSAAMMMMMMXXMASMAASMASASAS
XMSAMXAAMAXMXSXMMSAMXASXSXMAXSAMXAASAMXAMSAMXMSMMAAMSASASAAXMMSMMAMSAAAXMAMSAMXAAAASASASAAXXXXSASAAXMSAXMAASASAMAXMMXSSXXMASAXXAMXAMXXXMXSMM
AXMXMSSSSMSMXMASAMXSMMMXAXMAMSASXSMMMSAXMSXXAXMASXSXSMMMSMSMXAMXSXAMSSXMSXSSXXSSMMMSAMXSXMSAAXSASMSMXSASMSXMASMSSMXMAMMAMXASASXMASAXSAMXXMAS
";
}