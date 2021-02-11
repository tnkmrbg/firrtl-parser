use super::astbuilder::Rule;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
  #[error("Rule error: {0:?}")]
  Unexpected(Rule),
  #[error("No inner element")]
  NoInner,
  #[error("Parse error: {0:?}")]
  Parse(usize),
  #[error("pest Parse error: {0:?}")]
  PestParse(pest::error::Error<Rule>),
}
