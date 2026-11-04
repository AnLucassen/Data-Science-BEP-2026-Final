from pysubgroup.measures import AbstractInterestingnessMeasure, \
    BoundedInterestingnessMeasure

import numpy as np
import time


class SEMTarget(object):
    statistic_types = ['size_sg']

    def __init__(self):
        pass

    def __repr__(self):
        return "T: Structural Equation Model"

    def __eq__(self, other):
        return self.__dict__ == other.__dict__

    def __lt__(self, other):
        return str(self) < str(other)

    def get_attributes(self):
        return [] 

    def calculate_statistics(self, subgroup, data, stats):
        sg_instances = subgroup.covers(data)

        statistics = dict()
        statistics['size_sg'] = np.sum(sg_instances)
        return statistics

class SEM_QF(BoundedInterestingnessMeasure):
    def __init__(self, keep_log=False):
        #counters (always) and a per-candidate log (only if asked)
        self.keep_log = keep_log
        self.n_evaluated = 0     # every description the algorithm scores
        self.n_below_30 = 0      # rejected because it covers fewer than 30 rows
        self.log = []            # one entry per description (if keep_log)

    # musste ich einfuegen. ist das neu?
    def calculate_constant_statistics(self, data, target):
        pass

    def calculate_statistics(self, subgroup, target, data):
        pass
      
    # added: Apriori needs an upper bound. Infinity = never skip anything,
    # so all algorithms stay exhaustive.
    def optimistic_estimate(self, subgroup, target, data, statistics):
        return float("inf")

#puts -1 in rval adn returns it at the end, first it stopped early with return -1
    def evaluate(self, subgroup, target, data, statistics):
        # BEP week 42: same steps as the original, plus counting and timing
        self.n_evaluated += 1
        t_start = time.perf_counter()

        instances = subgroup.covers(data)
        variables = [str(selector.attribute_name) for selector in subgroup.selectors]
        size = int(instances.sum())

        if size < 30:
            self.n_below_30 += 1
            rval = -1
        else:
            rval = f_fit(instances, variables)

        if self.keep_log:
            self.log.append({
                "order": self.n_evaluated,           # 1st, 2nd, ... description scored
                "description": str(subgroup),       # e.g. school=='Pasteur'
                "depth": len(subgroup.selectors),   # number of conditions
                "size": size,                       # rows covered
                "quality": float(rval),             # -1 = too small or failed fit
                "seconds": time.perf_counter() - t_start,
            })
        return rval

    def is_applicable(self, subgroup):
        return isinstance(subgroup.target, SEMTarget)

    def supports_weights(self):
        return False

