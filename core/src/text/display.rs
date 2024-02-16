use std::fmt;

pub struct DisplayCommaSeparated<'a, T>(pub &'a [T]);
impl<'a, T: fmt::Display> fmt::Display for DisplayCommaSeparated<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }
            write!(f, "'{item}'")?;
        }

        Ok(())
    }
}
