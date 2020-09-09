use scraper::{Html, Selector, ElementRef};
use crate::instr::BitsElement::{Var, Fixed};
use crate::bitvec::BitVec;

#[derive(Debug)]
enum BitsElement {
    Var {
        len: u16,
        name: String,
    },
    Fixed {
        value: Vec<bool>
    },
}

#[derive(Debug)]
pub struct Instr {
    source: Html,
    pub id: String,
}



fn read_bitfield(element: &ElementRef) -> Vec<BitsElement> {
    element.children().filter_map(|it|
        it.value().as_element().map(|node| (node, it.children())))
        .map(|(node, child)| {
            let width = node.attr("width")
                .map_or(1u16, |len| len.parse::<u16>().unwrap());
            let internals: Vec<String> = child.map(|it| {
                let strings: Vec<_> = it.descendants()
                    .filter_map(|it| it.value().as_text())
                    .map(|text| text.trim().to_string())
                    .collect();
                strings.join("")
            }).collect();
            let string: String = internals.join("");
            let vec: Vec<_> =
                string.parse::<BitVec>().unwrap().into();
            if vec.len() == 0 {
                Var {
                    len: width,
                    name: node.attr("name").unwrap().to_string(),
                }
            } else {
                Fixed {
                    value: vec
                }
            }
        }).collect()
}

fn read_instr(html: Html) -> Option<Instr> {
    let inst =
        Selector::parse("instructionsection").unwrap();
    let operation =
        Selector::parse("ps[secttype=Operation]").unwrap();
    let bits =
        Selector::parse("regdiagram").unwrap();
    let id = html.select(&inst).last()
        .map(|item| item.value().attr("id").unwrap().to_string());
    let operation = html.select(&operation).last()
        .map(|it| it.text());
    let bit_field = html.select(&bits).last();
    let bits = bit_field.iter().map(|it|
        read_bitfield(it)
    ).last();
    id.map(|id| Instr {
        source: html,
        id,
    })
}

pub fn load_instructions<'a, T: Iterator<Item=String>>(files: T) -> Vec<Instr> {
    files.filter_map(|mut file| {
        let spec = Html::parse_document(file.as_str());
        read_instr(spec)
    }).collect()
}
