    function flatten_mapping(from, to) {
        function find_map(value) { 
            for (const [idx, m_t] of to.entries()) {
                const maybeMapping = m_t.maps_to(value);
                if (maybeMapping !== null) {
                    return [idx, m_t];
                }
            }
            return [-1, null];
        }
        let flattened = [];
        for (const m_f of from) { 
            const lower_0 = m_f.src;
            const upper_0 = m_f.src + m_f.len;

            const lower_1 = m_f.dest;
            const upper_1 = m_f.dest + m_f.len;
            
            const maybe_found = find_map(lower_1);
            if (maybe_found[0] === -1) {
                const lower_2 = lower_1;
                
            }

            let ft_dest = false;
            for (const [idx, m_t] of to.entries()) { 
                const maybePath = m_t.maps_to(f_dest);
                if (maybePath !== null) {
                    ft_dest = maybePath;
                    t_src_upper = m_t.src + m_t.len;
                    ft_dest_l
                }
            }
        }
    }
