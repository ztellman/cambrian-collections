package clojure.lang;

import java.util.Iterator;

public class PersistentVector4 extends APersistentVector implements IObj,
	IEditableCollection, IReduce {
    final Object e0;
    final Object e1;
    final Object e2;
    final Object e3;
    private final IPersistentMap meta;
    private int hash = -1;
    private int hasheq = -1;

    PersistentVector4(IPersistentMap meta, Object e0, Object e1, Object e2,
	    Object e3) {
	this.meta = meta;
	this.e0 = e0;
	this.e1 = e1;
	this.e2 = e2;
	this.e3 = e3;
    }

    public PersistentVector4(Object e0, Object e1, Object e2, Object e3) {
	this.meta = null;
	this.e0 = e0;
	this.e1 = e1;
	this.e2 = e2;
	this.e3 = e3;
    }

    public IPersistentMap meta() {
	return meta;
    }

    public IObj withMeta(IPersistentMap meta) {
	return new PersistentVector4(meta, e0, e1, e2, e3);
    }

    public Object nth(int i) {
	switch (i) {
	case 0:
	    return e0;
	case 1:
	    return e1;
	case 2:
	    return e2;
	case 3:
	    return e3;
	default:
	    throw new IndexOutOfBoundsException();
	}
    }

    public Object nth(int i, Object notFound) {
	switch (i) {
	case 0:
	    return e0;
	case 1:
	    return e1;
	case 2:
	    return e2;
	case 3:
	    return e3;
	default:
	    return notFound;
	}
    }

    public int count() {
	return 4;
    }

    public IPersistentVector empty() {
	return PersistentVector0.EMPTY;
    }

    public IPersistentVector assocN(int i, Object val) {
	switch (i) {
	case 0:
	    return new PersistentVector4(meta, val, e1, e2, e3);
	case 1:
	    return new PersistentVector4(meta, e0, val, e2, e3);
	case 2:
	    return new PersistentVector4(meta, e0, e1, val, e3);
	case 3:
	    return new PersistentVector4(meta, e0, e1, e2, val);
	case 4:
	    return cons(val);
	default:
	    throw new IndexOutOfBoundsException();
	}
    }

    public IPersistentVector cons(Object val) {
	return new PersistentVector5(meta, e0, e1, e2, e3, val);
    }

    public ITransientCollection asTransient() {
	ITransientCollection coll = PersistentVector.EMPTY.asTransient();
	return (ITransientVector) coll.conj(e0).conj(e1).conj(e2).conj(e3);
    }

    public IPersistentVector pop() {
	return new PersistentVector3(meta, e0, e1, e2);
    }

    public Object kvreduce(IFn f, Object init) {
	init = f.invoke(init, 0, e0);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, 1, e1);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, 2, e2);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, 3, e3);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	return init;
    }

    public Object reduce(IFn f) {
	Object init = e0;
	init = f.invoke(init, e1);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, e2);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, e3);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	return init;
    }

    public Object reduce(IFn f, Object init) {
	init = f.invoke(init, e0);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, e1);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, e2);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, e3);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	return init;
    }

    public int hashCode() {
	if (this.hash == -1) {
	    int hash = 1;
	    hash = (31 * hash) + (e0 == null ? 0 : e0.hashCode());
	    hash = (31 * hash) + (e1 == null ? 0 : e1.hashCode());
	    hash = (31 * hash) + (e2 == null ? 0 : e2.hashCode());
	    hash = (31 * hash) + (e3 == null ? 0 : e3.hashCode());
	    this.hash = hash;
	}
	return hash;
    }

    public int hasheq() {
	if (this.hasheq == -1) {
	    int hash = 1;
	    hash = (31 * hash) + Util.hasheq(e0);
	    hash = (31 * hash) + Util.hasheq(e1);
	    hash = (31 * hash) + Util.hasheq(e2);
	    hash = (31 * hash) + Util.hasheq(e3);
	    hash = Murmur3.mixCollHash(hash, 4);
	    this.hasheq = hash;
	}
	return hasheq;
    }

    public boolean equals(Object o) {
	if (o instanceof PersistentVector4) {
	    return Util.equals(e0, ((PersistentVector4) o).e0)
		    && Util.equals(e1, ((PersistentVector4) o).e1)
		    && Util.equals(e2, ((PersistentVector4) o).e2)
		    && Util.equals(e3, ((PersistentVector4) o).e3);
	} else {
	    return super.equals(o);
	}
    }

    public boolean equiv(Object o) {
	if (o instanceof PersistentVector4) {
	    return Util.equiv(e0, ((PersistentVector4) o).e0)
		    && Util.equiv(e1, ((PersistentVector4) o).e1)
		    && Util.equiv(e2, ((PersistentVector4) o).e2)
		    && Util.equiv(e3, ((PersistentVector4) o).e3);
	} else {
	    return super.equiv(o);
	}
    }

    public Iterator iterator() {
	return new Iterator() {
	    int i = 0;

	    public boolean hasNext() {
		return i < 4;
	    }

	    public Object next() {
		return nth(i++);
	    }

	    public void remove() {
		throw new UnsupportedOperationException();
	    }
	};
    }

    public ISeq seq() {
	return IteratorSeq.create(iterator());
    }
}