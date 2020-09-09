use scraper::{Html, Selector, ElementRef};
use zip::ZipArchive;
use std::io::{BufReader, Read};
use scraper::element_ref::{Select, Text};
use zip::read::ZipFile;
use scraper::node::Element;

extern crate pest;
#[macro_use]
extern crate pest_derive;

mod parse;
mod bitvec;
mod instr;

pub use crate::parse::*;

use crate::pest::Parser;
use crate::bitvec::BitVec;
use crate::instr::load_instructions;

fn main() {
    parse::parse_operation(
        "if msbit == 63 then
u();"
    )
}

fn extract_main() {
    let zip = std::fs::File::open("A64_ISA_xml_v86A-2020-06.zip");
    let mut archive = ZipArchive::new(zip.unwrap());
    let file_select =
        regex::Regex::new(r"ISA_A64_xml_v86A-2020-06_OPT/[a-z_0-9]+\.xml")
            .unwrap();
    if let Ok(mut archive) = archive {
        let mut content = String::new();
        let instrs: Vec<_> =
            archive.file_names().filter(|name| file_select.is_match(name))
                .map(|it| it.to_string()).collect();
        let parsed = load_instructions(
            instrs[..2].into_iter().map(|file| {
                archive.by_name(file.as_str()).unwrap().read_to_string(&mut content);
                content.clone()
            })
        );
        let ids: Vec<_> = parsed.iter().map(|instr| instr.id.as_str()).collect();
        println!("{:?}", ids);
    }
}