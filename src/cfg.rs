use confy::ConfyError;
use log::{debug, trace};
use serde_derive::{Deserialize, Serialize};
use std::default::Default;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub(crate) enum BracketSizing {
    None,
    Incremental,
    LeftRight,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(crate) struct Cfg {
    pub(crate) latex_output: LatexOutputCfg,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct LatexOutputCfg {
    pub(crate) bracket_types: Vec<String>,
    pub(crate) bracket_sizing: BracketSizing,
    pub(crate) line_length: usize,
    pub(crate) line_break_before: Vec<String>,
    pub(crate) line_break_in_argument: bool,
    pub(crate) multiplication_symbol: String,
    pub(crate) align_at: Vec<String>,
    pub(crate) tags: bool,
    pub(crate) indent_with: String,
    pub(crate) subscript_size: f64,
}

impl Default for LatexOutputCfg {
    fn default() -> Self {
        Self {
            bracket_types: vec![
                "(".to_string(),
                ")".to_string(),
                "[".to_string(),
                "]".to_string(),
                "\\{".to_string(),
                "\\}".to_string(),
            ],
            bracket_sizing: BracketSizing::LeftRight,
            line_length: 70,
            line_break_before: vec!["+".to_string(), "-".to_string()],
            line_break_in_argument: false,
            multiplication_symbol: r"\,".to_string(),
            align_at: vec!["=".to_string()],
            tags: false,
            indent_with: "   ".to_string(),
            subscript_size: 0.7,
        }
    }
}

impl Cfg {
    fn load(name: &str) -> Result<Self, ConfyError> {
        debug!("loading config from {}", name);
        confy::load(name, None)
    }
}

lazy_static! {
    pub(crate) static ref CFG: Cfg = {
        let cfg = Cfg::load("mathtomath");
        trace!("read config: {:?}", cfg);
        cfg.unwrap()
    };
}
