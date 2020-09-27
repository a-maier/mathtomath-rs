use std::default::Default;
use confy::ConfyError;
use serde_derive::{Serialize, Deserialize};
use log::{debug, trace};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(crate) struct Cfg {
    pub(crate) latex_output: LatexOutputCfg,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct LatexOutputCfg {
    pub(crate) line_length: usize,
    pub(crate) line_break_before: Vec<String>,
    pub(crate) line_break_in_argument: bool,
    pub(crate) align_at: Vec<String>,
    pub(crate) tags: bool,
    pub(crate) indent_with: String,
    pub(crate) subscript_size: f64
}

impl Default for LatexOutputCfg {
    fn default() -> Self {
        Self{
            line_length: 80,
            line_break_before: vec!["+".to_string(), "-".to_string()],
            line_break_in_argument: false,
            align_at: vec!["=".to_string()],
            tags: false,
            indent_with: "   ".to_string(),
            subscript_size: 0.7
        }
    }
}

impl Cfg {
    fn load(name: &str) -> Result<Self, ConfyError> {
        debug!("loading config from {}", name);
        confy::load(name)
    }
}

lazy_static! {
    pub(crate) static ref CFG: Cfg = {
        let cfg = Cfg::load("mathtomath");
        trace!("read config: {:?}", cfg);
        cfg.unwrap()
    };
}
